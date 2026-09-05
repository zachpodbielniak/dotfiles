#!/usr/bin/env python3
"""Isolated security regressions; no services, containers, or user data required."""
import ast
import asyncio
import hashlib
import ipaddress
import json
import math
import os
from pathlib import Path
import re
import shlex
import string
import subprocess
import sys
import tempfile
import time
import textwrap
from fractions import Fraction
from types import SimpleNamespace
import unittest
from unittest.mock import Mock

ROOT = Path(__file__).resolve().parents[2]
SCRIPTS = ROOT / 'bin/scripts'


def definitions(script, *names, **namespace):
    """Load real definitions without running CLI initialization or service imports."""
    tree = ast.parse((SCRIPTS / script).read_text())
    nodes = [n for n in tree.body if isinstance(n, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)) and n.name in names]
    assert len(nodes) == len(names)
    module = ast.Module(body=ast.parse('from __future__ import annotations').body + nodes, type_ignores=[])
    exec(compile(module, str(SCRIPTS / script), 'exec'), namespace)
    return namespace


def shell_function(script, name):
    source = (SCRIPTS / script).read_text()
    match = re.search(r'^' + name + r'\s*\(\)\s*\{.*?^\}', source, re.M | re.S)
    assert match, name
    return match[0]


class SecurityRegressions(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix='script-audit-')
        self.addCleanup(self.temp.cleanup)
        self.work = Path(self.temp.name)
        self.bin = self.work / 'bin'
        self.bin.mkdir()
        self.env = {**os.environ, 'HOME': str(self.work), 'PATH': str(self.bin) + os.pathsep + os.environ['PATH'], 'NO_DBOX_CHECK': '1'}

    def run_script(self, name, *args, data='', **kwargs):
        return subprocess.run([str(SCRIPTS / name), *map(str, args)], input=data, text=True, capture_output=True, cwd=self.work, env=self.env, timeout=15, **kwargs)

    def bash(self, code, *args, data=''):
        return subprocess.run(['bash', '-c', code, 'test', *map(str, args)], input=data, text=True, capture_output=True, cwd=self.work, env=self.env, timeout=15)

    def stub(self, name, body):
        path = self.bin / name
        path.write_text('#!/usr/bin/env python3\n' + body)
        path.chmod(0o755)

    def ok(self, result):
        self.assertEqual(result.returncode, 0, result.stderr + result.stdout)
        return result.stdout

    def test_columns_selections_and_delimiters(self):
        self.assertEqual(self.ok(self.run_script('columns', '1,3-4', data='a b c d\n')), 'a\tc\td\n')
        self.assertEqual(self.ok(self.run_script('columns', '-d,', '-o|', '2-3', data='a,b,c\n')), 'b|c\n')
        self.assertEqual(self.ok(self.run_script('columns', '2', data='a\tb c\n')), 'b\n')
        self.assertEqual(self.ok(self.run_script('columns', '08', data='1 2 3 4 5 6 7 8\n')), '8\n')

    def test_columns_rejects_source_injection(self):
        for selection in ['1;system("touch PWN")', "1';touch PWN;#", '1,x', '3-1', '0', '1,', '100001']:
            with self.subTest(selection=selection):
                self.assertNotEqual(self.run_script('columns', selection, data='a b\n').returncode, 0)
        delim = "';touch PWN;#"
        self.ok(self.run_script('columns', '-o', delim, '1,2', data='a b\n'))
        self.assertFalse((self.work / 'PWN').exists())

    def test_dotenv_export_is_literal_and_formats_roundtrip(self):
        value = '$(touch PWN)`touch PWN` \\ path "quoted"'
        source = self.work / '.env'
        source.write_text('PAYLOAD=' + value + '\nSINGLE=\'a value\'\nBOOL=false\n')
        exports = self.ok(self.run_script('dotenv', source, '--export'))
        self.assertEqual(self.ok(self.bash(exports + '\nprintf %s "$PAYLOAD"')), value)
        self.assertFalse((self.work / 'PWN').exists())
        for fmt in ['--json', '--yaml']:
            self.assertEqual(json.loads(self.ok(self.run_script('dotenv', source, fmt))), {'PAYLOAD': value, 'SINGLE': 'a value', 'BOOL': 'false'})
        source.write_text('A="unterminated\n')
        self.assertNotEqual(self.run_script('dotenv', source, '--check').returncode, 0)

    def test_newest_oldest_paths_counts_and_large_stream(self):
        directory = self.work / '$(touch PWN)'
        directory.mkdir()
        for index, name in enumerate(['old file', 'middle\nfile', 'new file']):
            path = directory / name
            path.touch()
            os.utime(path, (100 + index, 100 + index))
        self.assertEqual(self.ok(self.run_script('newest', directory, '2')), f'{directory}/new file\n{directory}/middle\nfile\n')
        self.assertEqual(self.ok(self.run_script('oldest', directory)), f'{directory}/old file\n')
        for name in ['oldest', 'newest']:
            self.assertNotEqual(self.run_script(name, directory, '0').returncode, 0)
        self.assertFalse((self.work / 'PWN').exists())
        for index in range(1500):
            (directory / str(index)).touch()
        self.ok(self.run_script('newest', directory))

    def test_cache_failures_not_published_clear_and_ttl(self):
        command = 'printf partial; exit 7'
        for _ in range(2):
            result = self.run_script('cache_cmd', '1h', command)
            self.assertEqual(result.returncode, 7)
        command = 'printf x >> calls; printf complete'
        for _ in range(2):
            self.assertEqual(self.ok(self.run_script('cache_cmd', '08m', command)), 'complete')
        self.assertEqual((self.work / 'calls').read_text(), 'x')
        self.ok(self.run_script('cache_cmd', '--clear', command))
        self.ok(self.run_script('cache_cmd', '1h', command))
        self.assertEqual((self.work / 'calls').read_text(), 'xx')
        self.assertNotEqual(self.run_script('cache_cmd', 'a[$(touch PWN)]', command).returncode, 0)
        self.assertFalse((self.work / 'PWN').exists())

    def test_cache_symlinks_and_clear_scope(self):
        cache = self.work / 'cache'
        cache.mkdir(mode=0o700)
        victim = self.work / 'victim'
        victim.write_text('keep')
        command = 'printf replacement'
        key = hashlib.sha256((str(self.work) + '\0' + command).encode()).hexdigest()
        entry = cache / key
        entry.symlink_to(victim)
        self.assertEqual(self.ok(self.run_script('cache_cmd', '--cache-dir', cache, '1h', command)), 'replacement')
        self.assertEqual(victim.read_text(), 'keep')
        self.assertFalse(entry.is_symlink())
        (cache / 'unrelated').write_text('keep')
        self.ok(self.run_script('cache_cmd', '--cache-dir', cache, '--clear-all'))
        self.assertTrue((cache / 'unrelated').exists())
        self.assertFalse(entry.exists())
        link = self.work / 'link'
        link.symlink_to(cache)
        self.assertNotEqual(self.run_script('cache_cmd', '--cache-dir', link, '--clear-all').returncode, 0)

    def test_bulk_rename_quoted_names_and_first_iteration(self):
        source = self.work / "source';touch PWN;#"
        source.write_text('payload')
        dest = self.work / "dest' with spaces"
        self.env['DEST'] = str(dest)
        self.stub('vipe', "import os,sys\ndata=sys.stdin.read()\nprint(data.replace('exit 255', ':') if '--suffix' in sys.argv else os.environ['DEST'], end='' if '--suffix' in sys.argv else '\\n')\n")
        self.ok(self.run_script('bulk_rename', source))
        self.assertEqual(dest.read_text(), 'payload')
        self.assertFalse(source.exists())
        self.assertFalse((self.work / 'PWN').exists())

    def test_for_all_files_preserves_args_empty_dirs_and_failures(self):
        directory = self.work / 'empty'
        directory.mkdir()
        self.stub('capture', "import json,sys\nprint(json.dumps(sys.argv[1:]))\n")
        code = 'cd "$1"; shift; "$@"'
        self.assertEqual(self.ok(self.bash(code, directory, SCRIPTS / 'for_all_files', 'capture')), '')
        (directory / 'a file').touch()
        result = self.bash(code, directory, SCRIPTS / 'for_all_files', 'capture', 'before space', '--', '*')
        self.assertEqual(json.loads(self.ok(result)), ['before space', './a file', '*'])
        self.stub('capture', 'import sys\nsys.exit(9)\n')
        self.assertEqual(self.bash(code, directory, SCRIPTS / 'for_all_files', 'capture').returncode, 9)

    def test_wrappers_preserve_first_and_quoted_arguments(self):
        for script, target, prefix in [('255colors', '255colors.hs', []), ('template_journal', 'template_journal.hs', []), ('b', 'brightnessctl', ['s']), ('clapper-launcher', 'flatpak', ['--user', 'run', 'com.github.rafostar.Clapper'])]:
            with self.subTest(script=script):
                self.stub(target, 'import json,sys\nprint(json.dumps(sys.argv[1:]))\n')
                self.assertEqual(json.loads(self.ok(self.run_script(script, 'first arg', '*'))), prefix + ['first arg', '*'])
        self.stub('watch', 'import sys\nprint("watch started")\n')
        self.assertEqual(self.ok(self.run_script('get_cpu_frequency')), 'watch started\n')
        (self.work / '.bashrc').write_text('myfunc() { printf "%s" "$1"; }\n')
        self.assertEqual(self.ok(self.run_script('bash_run', 'myfunc', '$(touch PWN)')), '$(touch PWN)')
        self.assertFalse((self.work / 'PWN').exists())

    def test_distrobox_launchers_propagate_failure(self):
        self.stub('distrobox', 'import sys\nsys.exit(23)\n')
        self.env.pop('NO_DBOX_CHECK')
        self.env.pop('CONTAINER_ID', None)
        count = 0
        for path in SCRIPTS.iterdir():
            if re.search(r'(?:sys\.)?exit\((?:subprocess\.)?run\(cmd\).returncode\)', '\n'.join(path.read_text().splitlines()[:160])):
                with self.subTest(script=path.name):
                    result = self.run_script(path.name)
                    self.assertEqual(result.returncode, 23, result.stderr)
                count += 1
        self.assertGreaterEqual(count, 62)

    def test_sbi_summary_arguments_and_passthrough(self):
        self.stub('ai_summary_as_markdown', 'import json,sys\nprint(json.dumps(sys.argv[1:]))\nprint(sys.stdin.read(), end="")\n')
        setup = shell_function('sbi', 'run_ai_summary') + '\ndebug_log() { :; }\n'
        values = {'NEORG_MODE': 'false', 'NO_SUMMARY_MODE': 'false', 'SUMMARY_TYPE': 'a type', 'SUMMARY_PROMPT': '$(touch PWN)', 'SUMMARY_PRINCIPLE_MODE': 'false', 'DEBUG_MODE': 'false', 'SUMMARY_PROVIDER': '', 'SUMMARY_MODEL': 'a model', 'SANITIZE_MODE': 'false'}
        setup += '\n'.join(f'{k}={shlex.quote(v)}' for k, v in values.items())
        result = self.bash(setup + '\nrun_ai_summary', data='body')
        output = self.ok(result).splitlines()
        self.assertEqual(json.loads(output[0]), ['--type', 'a type', '--prompt', '$(touch PWN)', '--model', 'a model'])
        self.assertEqual(output[1], 'body')
        path = self.work / '$(touch PWN)'
        path.write_text('file body')
        self.assertEqual(self.ok(self.bash(setup + '\nNO_SUMMARY_MODE=true\nrun_ai_summary "$1"', path)), 'file body')
        out = self.work / 'out'
        self.ok(self.bash(setup + '\nNO_SUMMARY_MODE=true\nrun_ai_summary /dev/stdin "$1"', out, data='stdin body'))
        self.assertEqual(out.read_text(), 'stdin body')
        self.assertFalse((self.work / 'PWN').exists())
        self.assertNotEqual(self.bash(setup + '\nSANITIZE_MODE=true\nsanitize_data() { return 6; }\nrun_ai_summary', data='body').returncode, 0)

    def test_formula_functions_and_cell_data(self):
        ns = definitions('md_table_calc', 'evaluate_formula', 'MarkdownTableCalculator', ast=ast, re=re, math=math, string=string)
        calculator = ns['MarkdownTableCalculator']()
        calculator.parse_table(['| name | amount |', '| --- | --- |', '| A | 12 |', '| B | 8 |'])
        self.assertEqual(calculator.evaluate_expression('SUM(B)'), 20)
        self.assertEqual(calculator._evaluate_row_expression('$_amount * 2', 0), 24)
        self.assertEqual(calculator._evaluate_row_expression('$_name', 0), 'A')
        self.assertEqual(calculator._evaluate_row_expression('"B" + A', 0), 'BA')
        payload = '\" + __import__(\'os\').system(\'touch PWN\') + \"'
        calculator.data[0][0] = payload
        self.assertEqual(calculator._evaluate_row_expression('$_name', 0), payload)
        for expression in ["__import__('os')", 'abs.__globals__', '().__class__.__bases__', '(lambda: 1)()', '[x for x in [1]]']:
            with self.subTest(expression=expression), self.assertRaises(ValueError):
                calculator.evaluate_expression(expression)

    def test_webhook_peer_cannot_be_spoofed(self):
        request = SimpleNamespace(remote_addr='203.0.113.10', headers={'X-Forwarded-For': '127.0.0.1', 'X-Real-IP': '127.0.0.1'}, path='/')
        ns = definitions('webhook_cmd', 'get_client_ip', 'check_ip_in_list', 'is_ip_whitelisted', 'check_global_whitelist', request=request, config={'whitelist': ['127.0.0.1']}, ipaddress=ipaddress, debug_mode=False, jsonify=lambda obj: obj, app=SimpleNamespace(before_request=lambda f:f))
        self.assertEqual(ns['get_client_ip'](), '203.0.113.10')
        self.assertEqual(asyncio.run(ns['check_global_whitelist']())[1], 403)
        request.remote_addr = '127.0.0.1'
        self.assertIsNone(asyncio.run(ns['check_global_whitelist']()))
        config_path = self.work / 'webhooks.json'
        config_path.write_text('{}')
        loader = definitions('webhook_cmd', 'load_config', CONFIG_PATH=config_path, yaml=SimpleNamespace(safe_load=json.load))
        loader['load_config']()
        self.assertEqual(loader['config']['host'], '127.0.0.1')
        self.assertEqual(loader['config']['whitelist'], ['127.0.0.1', '::1'])
        config_path.write_text('{"host": "0.0.0.0", "whitelist": []}')
        loader['load_config']()
        self.assertEqual(loader['config']['host'], '0.0.0.0')
        self.assertEqual(loader['config']['whitelist'], [])
        config_path.write_text('[]')
        with self.assertRaises(ValueError):
            loader['load_config']()

    def test_queue_whitelist_uses_argv_without_shell(self):
        ns = definitions('queue_command', 'is_command_trusted', 'Worker', shlex=shlex, subprocess=subprocess, load_trusted_commands=lambda _: ['printf'])
        config = SimpleNamespace(trust_all=False)
        worker = object.__new__(ns['Worker'])
        worker.config = config
        command = 'printf %s "$(touch ' + str(self.work / 'PWN') + ')"'
        self.assertTrue(ns['is_command_trusted'](command, config))
        output, error, status = worker._execute_command(command)
        self.assertEqual(status, 0, error)
        self.assertIn('$(touch', output)
        self.assertFalse((self.work / 'PWN').exists())
        self.assertFalse(ns['is_command_trusted']('printf "unterminated', config))
        config.trust_all = True
        self.assertEqual(worker._execute_command('printf one; printf two')[0], 'onetwo')
        parser = definitions('queue_command', 'parse_args', Config=SimpleNamespace, os=SimpleNamespace(environ={}), shlex=shlex)
        argv = ['printf', '%s', 'quoted argument', '*']
        queued = parser['parse_args'](['--', *argv]).command_to_queue
        self.assertEqual(shlex.split(queued), argv)

    def test_media_frame_rate_is_data(self):
        response = SimpleNamespace(returncode=0, stdout='')
        ns = definitions('media', 'extract_metadata_ffprobe', subprocess=SimpleNamespace(run=lambda *a, **k: response), json=json, Fraction=Fraction, debug_print=lambda *a: None)
        for rate, expected in [('30000/1001', 30000/1001), ('0/0', 0), ("__import__('os').system('touch PWN')", 0)]:
            response.stdout = json.dumps({'streams': [{'codec_type': 'video', 'r_frame_rate': rate}]})
            self.assertEqual(ns['extract_metadata_ffprobe']('video')['fps'], expected)

    def test_record_screen_preserves_output_and_codec_args(self):
        self.stub('ffmpeg', 'import json,sys\nprint(json.dumps(sys.argv[1:]))\nsys.exit(7)\n')
        code = shell_function('record_screen', 'record_screen') + '\nget_display_geometry() { echo 1920x1080+0+0; }\nstop_camera_preview() { :; }\n'
        code += 'SCREEN=screen FRAMERATE=30 RECORD_AUDIO=true AUDIO_DEVICE="device name" VIDEO_CODEC=gif AUDIO_CODEC=opus OUTPUT_FILE="$1" SHOW_CAMERA=false RED= GREEN= NC=\nrecord_screen'
        result = self.bash(code, '$(touch PWN).gif')
        self.assertEqual(result.returncode, 7, result.stderr)
        argv = json.loads(result.stdout.splitlines()[-1])
        self.assertEqual(argv[-1], '$(touch PWN).gif')
        self.assertIn('device name', argv)
        self.assertIn('palettegen', argv[argv.index('-vf') + 1])
        self.assertFalse((self.work / 'PWN').exists())

    def test_reddit_cache_replaces_symlink_atomically(self):
        cache = self.work / 'cache'
        cache.mkdir()
        ns = definitions('rdt', 'get_cache_path', 'save_to_cache', CACHE_DIR=str(cache), hashlib=hashlib, Path=Path, tempfile=tempfile, os=os, json=json, time=time, debug=lambda *a: None)
        victim = self.work / 'victim'
        victim.write_text('keep')
        target = ns['get_cache_path']('url')
        target.symlink_to(victim)
        ns['save_to_cache']('url', {'ok': True})
        self.assertEqual(victim.read_text(), 'keep')
        self.assertEqual(json.loads(target.read_text())['data'], {'ok': True})
        self.assertEqual(target.stat().st_mode & 0o777, 0o600)

    def test_email_msg_path_is_an_argument(self):
        runner = Mock(return_value=SimpleNamespace(returncode=1, stderr='mock extraction failure'))
        ns = definitions('email_to_markdown', 'parse_msg', has_extract_msg=True, Path=Path, tempfile=tempfile, run=runner, debug_print=lambda *a: None)
        filename = "mail';touch PWN;#.msg"
        with self.assertRaises(RuntimeError):
            ns['parse_msg'](filename)
        self.assertEqual(runner.call_args.args[0][0:2], ['extract_msg', filename])
        self.assertFalse(runner.call_args.kwargs.get('shell', False))

    def test_summary_provider_argv(self):
        source = (SCRIPTS / 'ai_summary_as_markdown').read_text()
        start = source.index('    # Build the command using aipy')
        end = source.index('    if (True == args.debug):', start)
        args = SimpleNamespace(model='model; touch PWN', provider=None, no_preserve=True, emojis=3, enhance_emojis=True, files=['a file; touch PWN'], images=['image name'])
        ns = {'args': args}
        exec(textwrap.dedent(source[start:end]), ns)
        self.assertEqual(ns['provider_cmd'], ['aipy', '--model', args.model, '--no-preserve', '--emojis', '3', '--enhance-emojis', '--file', args.files[0], '--image', args.images[0]])
        start = source.index('    result = run(\n        provider_cmd,')
        end = source.index('    # Handle the results', start)
        runner = Mock()
        ns.update(run=runner, query='body')
        exec(textwrap.dedent(source[start:end]), ns)
        self.assertEqual(runner.call_args.args[0], ns['provider_cmd'])
        self.assertFalse(runner.call_args.kwargs.get('shell', False))

    def test_devcontainer_metadata_is_quoted_at_shell_boundaries(self):
        functions = ['generate_distrobox_command', 'parse_container_env', 'parse_mounts', 'parse_capabilities', 'parse_security_opts', 'parse_run_args']
        code = '\n'.join(shell_function('dbox_from_devcontainer', name) for name in functions)
        code += '\nlog_debug() { :; }; log_error() { echo "$*" >&2; }; parse_init() { :; }; parse_gpu_requirements() { :; }; parse_privileged() { :; }\n'
        self.stub('distrobox', 'import json,sys\nprint(json.dumps(sys.argv[1:]))\n')
        self.stub('yq', "import os,sys\nq=sys.argv[2]\nprint(os.environ['PAYLOAD'] if any(key in q for key in ['containerEnv', 'capAdd', 'securityOpt', 'runArgs']) else '')\n")
        self.env['PAYLOAD'] = "KEY=it's $(touch PWN)"
        result = self.bash(code + '\ncmd=$(generate_distrobox_command config "$1" "$2" .)\neval "$cmd"', "container';touch PWN;#", "image';touch PWN;#")
        argv = json.loads(self.ok(result))
        self.assertEqual(argv[:5], ['create', '--name', "container';touch PWN;#", '--image', "image';touch PWN;#"])
        # Simulate the additional runtime flags shell boundary as well.
        for index, arg in enumerate(argv):
            if arg == '--additional-flags':
                self.ok(self.bash('eval "set -- $1"\nprintf "%s\\n" "$@"', argv[index + 1]))
        self.assertFalse((self.work / 'PWN').exists())

    def test_git_link_metadata_does_not_execute_url(self):
        code = shell_function('extract_git_links', 'extract_metadata')
        code += '\nextract_metadata "$1"\nprintf "%s\\n" "$org_name" "$repo_name"'
        payload = 'https://github.com/$(touch PWN)/repo'
        self.assertEqual(self.ok(self.bash(code, payload)), '$(touch PWN)\nrepo\n')
        self.assertFalse((self.work / 'PWN').exists())
        result = self.run_script('extract_git_links', '--format', 'json', data='https://github.com/example/repo\n')
        self.assertEqual(json.loads(self.ok(result))[0]['repo_name'], 'repo')

    def test_thread_dump_remote_args_and_temporary_file(self):
        # Run only the remote forwarding function, with a local SSH stub.
        harness = self.work / 'remote-harness'
        harness.write_text(shell_function('thread_dump', 'run_remote') + '\nrun_remote "$@"\n')
        self.stub('ssh', 'import json,sys\nprint(json.dumps(sys.argv[1:]))\n')
        result = self.bash('source "$1" host "$2" "$3"', harness, "arg' with spaces", '$(touch PWN)')
        argv = json.loads(self.ok(result))
        self.assertEqual(shlex.split(argv[-1]), ['bash', '-s', '--', "arg' with spaces", '$(touch PWN)'])
        self.assertFalse((self.work / 'PWN').exists())
        code = shell_function('thread_dump', 'print_malloc_info')
        code += '\nopt_sudo=false\nwarn() { :; }\nrun_gdb_with_cmds() { printf %s "$tmp_file" > "$HOME/temp-path"; printf "<heap/>" > "$tmp_file"; }\nprint_malloc_info 123'
        self.assertIn('<heap/>', self.ok(self.bash(code)))
        self.assertFalse(Path((self.work / 'temp-path').read_text()).exists())


if __name__ == '__main__':
    result = unittest.main(verbosity=2, exit=False).result
    if not result.wasSuccessful():
        sys.exit(1)
    print('security regression checks passed')
