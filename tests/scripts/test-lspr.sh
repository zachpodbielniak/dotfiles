#!/bin/bash

# dotfiles - Personal configuration files and scripts
# Copyright (C) 2026  Zach Podbielniak
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

# Fixture tests for lspr. Curl, fzf, and xdg-open are stubs. No network.

set -euo pipefail

SCRIPT="$(readlink -f "$(dirname "${BASH_SOURCE[0]}")/../../bin/scripts/lspr")"
ROOT="$(mktemp -d "${TMPDIR:-/tmp}/lspr-test.XXXXXX")"
PASS=0
FAIL=0

cleanup () {
	rm -rf "${ROOT}"
}
trap cleanup EXIT

FIX="${ROOT}/fix"
STUB_BIN="${ROOT}/bin"
mkdir -p "${FIX}" "${STUB_BIN}"
CURL_LOG="${ROOT}/curl.log"
OPEN_LOG="${ROOT}/open.log"
ERR="${ROOT}/err"
OUT="${ROOT}/out"
: > "${CURL_LOG}"
: > "${OPEN_LOG}"

cat > "${FIX}/fj_user.json" << 'EOF'
{"login":"zach"}
EOF
cat > "${FIX}/fj_orgs.json" << 'EOF'
[{"username":"widgets"}]
EOF
cat > "${FIX}/fj_open.json" << 'EOF'
[{
  "number": 13,
  "title": "feat site actions",
  "html_url": "https://git.podbielniak.com/zach/libresite/pulls/13",
  "state": "open",
  "updated_at": "2026-10-04T14:24:17Z",
  "user": {"login": "alice"},
  "draft": false,
  "pull_request": {"merged": false, "draft": false},
  "repository": {"full_name": "zach/libresite"}
}]
EOF
cat > "${FIX}/fj_closed.json" << 'EOF'
[
  {
    "number": 4,
    "title": "old closed",
    "html_url": "https://git.podbielniak.com/zach/libresite/pulls/4",
    "state": "closed",
    "updated_at": "2026-03-01T00:00:00Z",
    "user": {"login": "alice"},
    "pull_request": {"merged": false},
    "repository": {"full_name": "zach/libresite"}
  },
  {
    "number": 5,
    "title": "old merged",
    "html_url": "https://git.podbielniak.com/zach/libresite/pulls/5",
    "state": "closed",
    "updated_at": "2026-04-01T00:00:00Z",
    "user": {"login": "alice"},
    "pull_request": {"merged": true},
    "repository": {"full_name": "zach/libresite"}
  }
]
EOF
cat > "${FIX}/fj_issues.json" << 'EOF'
[{
  "number": 7,
  "title": "broken build",
  "html_url": "https://git.podbielniak.com/zach/libresite/issues/7",
  "state": "open",
  "updated_at": "2026-10-01T00:00:00Z",
  "user": {"login": "ada"},
  "pull_request": null,
  "repository": {"full_name": "zach/libresite"}
}]
EOF
cat > "${FIX}/fj_review.json" << 'EOF'
[
  {
    "number": 8,
    "title": "please review",
    "html_url": "https://forgejo.test/widgets/demo/pulls/8",
    "state": "open",
    "updated_at": "2026-09-01T00:00:00Z",
    "user": {"login": "ada"},
    "pull_request": {"merged": false, "draft": false},
    "repository": {"full_name": "widgets/demo"}
  },
  {
    "number": 2,
    "title": "other review",
    "html_url": "https://forgejo.test/other/repo/pulls/2",
    "state": "open",
    "updated_at": "2026-09-02T00:00:00Z",
    "user": {"login": "ada"},
    "pull_request": {"merged": false, "draft": false},
    "repository": {"full_name": "other/repo"}
  }
]
EOF
cat > "${FIX}/fj_view_issue.json" << 'EOF'
{
  "number": 9,
  "title": "repo issue",
  "body": "the build is red\n\nsecond paragraph",
  "html_url": "https://forgejo.test/widgets/demo/issues/9",
  "state": "open",
  "created_at": "2026-06-01T00:00:00Z",
  "updated_at": "2026-07-01T00:00:00Z",
  "user": {"login": "ada"},
  "labels": [{"name": "bug"}],
  "assignees": [{"login": "zach"}]
}
EOF
cat > "${FIX}/fj_view_comments.json" << 'EOF'
[{
  "user": {"login": "ben"},
  "created_at": "2026-07-02T00:00:00Z",
  "body": "I can reproduce this."
}]
EOF
cat > "${FIX}/fj_view_pull.json" << 'EOF'
{
  "number": 9,
  "title": "repo issue",
  "body": "the build is red",
  "html_url": "https://forgejo.test/widgets/demo/pulls/9",
  "state": "open",
  "base": {"ref": "main"},
  "head": {"ref": "feature"}
}
EOF
cat > "${FIX}/fj_view_reviews.json" << 'EOF'
[]
EOF
cat > "${FIX}/gh_review.json" << 'EOF'
{
  "total_count": 1,
  "incomplete_results": false,
  "items": [{
    "number": 3,
    "title": "needs you",
    "html_url": "https://github.com/other/repo/pull/3",
    "state": "open",
    "updated_at": "2026-09-01T00:00:00Z",
    "user": {"login": "ada"},
    "pull_request": {}
  }]
}
EOF
cat > "${FIX}/gh_view_issue.json" << 'EOF'
{
  "number": 3,
  "title": "needs you",
  "body": "please look",
  "html_url": "https://github.com/widgets/demo/pull/3",
  "state": "open",
  "user": {"login": "ada"},
  "created_at": "2026-09-01T00:00:00Z",
  "updated_at": "2026-09-02T00:00:00Z",
  "pull_request": {"url": "https://github.test/repos/widgets/demo/pulls/3"}
}
EOF
cat > "${FIX}/gh_view_pull.json" << 'EOF'
{
  "number": 3,
  "title": "needs you",
  "body": "please look",
  "html_url": "https://github.com/widgets/demo/pull/3",
  "state": "open",
  "draft": false,
  "base": {"ref": "main"},
  "head": {"ref": "feature"},
  "user": {"login": "ada"}
}
EOF
cat > "${FIX}/gh_view_comments.json" << 'EOF'
[{
  "user": {"login": "ben"},
  "created_at": "2026-09-02T01:00:00Z",
  "body": "issue thread"
}]
EOF
cat > "${FIX}/gh_view_reviews.json" << 'EOF'
[{
  "user": {"login": "zach"},
  "state": "APPROVED",
  "submitted_at": "2026-09-02T02:00:00Z",
  "body": "LGTM"
}]
EOF
cat > "${FIX}/gh_view_rcomments.json" << 'EOF'
[{
  "user": {"login": "zach"},
  "created_at": "2026-09-02T03:00:00Z",
  "path": "main.c",
  "line": 10,
  "body": "nit here"
}]
EOF
cat > "${FIX}/gl_reviews.json" << 'EOF'
[{
  "iid": 4,
  "title": "please review mr",
  "web_url": "https://gitlab.com/other/repo/-/merge_requests/4",
  "state": "opened",
  "updated_at": "2026-09-02T00:00:00Z",
  "author": {"username": "ada"},
  "references": {"full": "other/repo!4"}
}]
EOF
cat > "${FIX}/gl_view_mr.json" << 'EOF'
{
  "iid": 4,
  "title": "please review mr",
  "description": "the change itself",
  "web_url": "https://gitlab.test/widgets/demo/-/merge_requests/4",
  "state": "opened",
  "source_branch": "feature",
  "target_branch": "main",
  "author": {"username": "ada"},
  "references": {"full": "widgets/demo!4"}
}
EOF
cat > "${FIX}/gl_view_notes.json" << 'EOF'
[
  {"author": {"username": "bot"}, "system": true, "body": "assigned to zach", "created_at": "2026-09-01T00:00:00Z"},
  {"author": {"username": "ben"}, "system": false, "body": "ship it", "created_at": "2026-09-03T00:00:00Z"}
]
EOF
cat > "${FIX}/gl_view_issue.json" << 'EOF'
{"iid": 1, "title": "unused", "description": "unused", "state": "opened", "web_url": "https://gitlab.test/widgets/demo/-/issues/1"}
EOF
cat > "${FIX}/fj_repo_issues.json" << 'EOF'
[{
  "number": 9,
  "title": "repo issue",
  "html_url": "https://forgejo.test/widgets/demo/issues/9",
  "state": "open",
  "updated_at": "2026-07-01T00:00:00Z",
  "user": {"login": "ada"}
}]
EOF
cat > "${FIX}/gh_issues.json" << 'EOF'
{
  "total_count": 1,
  "incomplete_results": false,
  "items": [{
    "number": 4,
    "title": "github issue",
    "html_url": "https://github.com/widgets/demo/issues/4",
    "state": "open",
    "updated_at": "2026-03-02T00:00:00Z",
    "user": {"login": "ada"}
  }]
}
EOF
cat > "${FIX}/gl_group_issues.json" << 'EOF'
[{
  "iid": 11,
  "title": "group issue",
  "web_url": "https://gitlab.com/immutablue/immutablue/-/issues/11",
  "state": "opened",
  "updated_at": "2026-05-01T00:00:00.000Z",
  "author": {"username": "ada"},
  "references": {"full": "immutablue/immutablue#11"}
}]
EOF
cat > "${FIX}/gl_issue_graphql.json" << 'EOF'
{
  "data": {
    "user": {
      "namespace": {
        "projects": {
          "pageInfo": {"hasNextPage": false, "endCursor": null},
          "nodes": [{
            "fullPath": "zach/clawtilla",
            "issues": {
              "nodes": [{
                "iid": "6",
                "title": "personal issue",
                "webUrl": "https://gitlab.com/zach/clawtilla/-/issues/6",
                "state": "opened",
                "updatedAt": "2026-08-02T00:00:00Z",
                "author": {"username": "zach"}
              }]
            }
          }]
        }
      }
    }
  }
}
EOF
cat > "${FIX}/fj_pulls.json" << 'EOF'
[
  {
    "number": 1,
    "title": "alice pr",
    "html_url": "https://forgejo.test/widgets/demo/pulls/1",
    "state": "open",
    "updated_at": "2026-01-01T00:00:00Z",
    "user": {"login": "alice"}
  },
  {
    "number": 2,
    "title": "mine pr",
    "html_url": "https://forgejo.test/widgets/demo/pulls/2",
    "state": "open",
    "updated_at": "2026-02-01T00:00:00Z",
    "user": {"login": "zach"}
  }
]
EOF
cat > "${FIX}/gh_user.json" << 'EOF'
{"login":"zach"}
EOF
cat > "${FIX}/gh_orgs.json" << 'EOF'
[
  {"state":"active","role":"admin","organization":{"login":"widgets"}},
  {"state":"active","role":"member","organization":{"login":"EpicGames"}}
]
EOF
cat > "${FIX}/gh_search.json" << 'EOF'
{
  "total_count": 1,
  "incomplete_results": false,
  "items": [{
    "number": 3,
    "title": "landing page",
    "html_url": "https://github.com/Linear-Byte/LinearByte-Landing-Page/pull/3",
    "state": "open",
    "updated_at": "2026-01-02T00:00:00Z",
    "user": {"login": "bob"},
    "draft": false,
    "pull_request": {}
  }]
}
EOF
cat > "${FIX}/gl_user.json" << 'EOF'
{"username":"zach","id":1}
EOF
cat > "${FIX}/gl_groups.json" << 'EOF'
[{"id":9,"full_path":"widgets"}]
EOF
cat > "${FIX}/gl_group_mrs.json" << 'EOF'
[{
  "iid": 173,
  "title": "fedora 44",
  "web_url": "https://gitlab.com/immutablue/immutablue/-/merge_requests/173",
  "state": "opened",
  "updated_at": "2026-05-26T13:28:38.046Z",
  "author": {"username": "Creplav"},
  "draft": false,
  "references": {"full": "immutablue/immutablue!173"}
}]
EOF
cat > "${FIX}/gl_project_mrs.json" << 'EOF'
[{
  "iid": 8,
  "title": "project mr",
  "web_url": "https://gitlab.com/widgets/demo/-/merge_requests/8",
  "state": "opened",
  "updated_at": "2026-06-01T00:00:00Z",
  "author": {"username": "ada"},
  "draft": false
}]
EOF
cat > "${FIX}/gl_graphql.json" << 'EOF'
{
  "data": {
    "user": {
      "namespace": {
        "projects": {
          "pageInfo": {"hasNextPage": false, "endCursor": null},
          "nodes": [{
            "fullPath": "zach/clawtilla",
            "mergeRequests": {
              "nodes": [{
                "iid": "2",
                "title": "claw fix",
                "webUrl": "https://gitlab.com/zach/clawtilla/-/merge_requests/2",
                "state": "opened",
                "updatedAt": "2026-08-01T00:00:00Z",
                "author": {"username": "zach"}
              }]
            }
          }]
        }
      }
    }
  }
}
EOF

cat > "${STUB_BIN}/curl" << 'EOF'
#!/bin/bash
set -euo pipefail

output=""
hdr=""
url=""
data_file=""

while [[ $# -gt 0 ]]
do
	case "${1}" in
		--output|--dump-header|--url|--user-agent|--header|--max-time|--data-binary|--write-out)
			case "${1}" in
				--output) output="${2}" ;;
				--dump-header) hdr="${2}" ;;
				--url) url="${2}" ;;
				--data-binary) data_file="${2#@}" ;;
			esac
			shift 2
			;;
		--silent|--show-error|--location)
			shift
			;;
		*)
			printf 'curl stub: unexpected arg %s\n' "${1}" >&2
			exit 97
			;;
	esac
done

if [[ -z "${output}" || -z "${hdr}" || -z "${url}" ]]
then
	printf 'curl stub: missing output, header, or url\n' >&2
	exit 96
fi

printf '%s\n' "${url}" >> "${LSPR_CURL_LOG}"
path="${url%%\?*}"
query="${url#*\?}"
if [[ "${query}" == "${url}" ]]
then
	query=""
fi

write_json () {
	cp "${1}" "${output}"
	printf 'HTTP/1.1 200 OK\r\nX-Total-Count: 1\r\n\r\n' > "${hdr}"
	printf '200'
}

if [[ "${LSPR_STUB_401:-}" == 1 ]]
then
	printf 'HTTP/1.1 401 Unauthorized\r\n\r\n' > "${hdr}"
	printf '%s\n' '{"message":"bad token"}' > "${output}"
	printf '401'
	exit 0
fi

case "${path}" in
	https://forgejo.test/api/v1/user)
		write_json "${LSPR_FIX}/fj_user.json"
		;;
	https://forgejo.test/api/v1/user/orgs)
		write_json "${LSPR_FIX}/fj_orgs.json"
		;;
	https://forgejo.test/api/v1/repos/issues/search)
		if [[ "${query}" == *review_requested=true* ]]
		then
			write_json "${LSPR_FIX}/fj_review.json"
		elif [[ "${query}" == *type=issues* ]]
		then
			write_json "${LSPR_FIX}/fj_issues.json"
		elif [[ "${query}" == *state=closed* || "${query}" == *state=all* ]]
		then
			write_json "${LSPR_FIX}/fj_closed.json"
		else
			write_json "${LSPR_FIX}/fj_open.json"
		fi
		;;
	https://forgejo.test/api/v1/repos/*/issues/*/comments)
		write_json "${LSPR_FIX}/fj_view_comments.json"
		;;
	https://forgejo.test/api/v1/repos/*/pulls/*/reviews)
		write_json "${LSPR_FIX}/fj_view_reviews.json"
		;;
	https://forgejo.test/api/v1/repos/*/pulls/*)
		write_json "${LSPR_FIX}/fj_view_pull.json"
		;;
	https://forgejo.test/api/v1/repos/*/issues/*)
		write_json "${LSPR_FIX}/fj_view_issue.json"
		;;
	https://forgejo.test/api/v1/repos/*/pulls)
		write_json "${LSPR_FIX}/fj_pulls.json"
		;;
	https://forgejo.test/api/v1/repos/*/issues)
		write_json "${LSPR_FIX}/fj_repo_issues.json"
		;;
	https://github.test/user)
		write_json "${LSPR_FIX}/gh_user.json"
		;;
	https://github.test/user/memberships/orgs)
		write_json "${LSPR_FIX}/gh_orgs.json"
		;;
	https://github.test/search/issues)
		if [[ "${query}" == *review-requested* ]]
		then
			write_json "${LSPR_FIX}/gh_review.json"
		elif [[ "${query}" == *is%3Aissue* ]]
		then
			write_json "${LSPR_FIX}/gh_issues.json"
		else
			write_json "${LSPR_FIX}/gh_search.json"
		fi
		;;
	https://github.test/repos/*/issues/*/comments)
		write_json "${LSPR_FIX}/gh_view_comments.json"
		;;
	https://github.test/repos/*/pulls/*/comments)
		write_json "${LSPR_FIX}/gh_view_rcomments.json"
		;;
	https://github.test/repos/*/pulls/*/reviews)
		write_json "${LSPR_FIX}/gh_view_reviews.json"
		;;
	https://github.test/repos/*/pulls/*)
		write_json "${LSPR_FIX}/gh_view_pull.json"
		;;
	https://github.test/repos/*/issues/*)
		write_json "${LSPR_FIX}/gh_view_issue.json"
		;;
	https://gitlab.test/api/v4/merge_requests)
		write_json "${LSPR_FIX}/gl_reviews.json"
		;;
	https://gitlab.test/api/v4/projects/*/issues/*/notes)
		write_json "${LSPR_FIX}/gl_view_notes.json"
		;;
	https://gitlab.test/api/v4/projects/*/merge_requests/*/notes)
		write_json "${LSPR_FIX}/gl_view_notes.json"
		;;
	https://gitlab.test/api/v4/projects/*/merge_requests/*)
		write_json "${LSPR_FIX}/gl_view_mr.json"
		;;
	https://gitlab.test/api/v4/projects/*/issues/*)
		write_json "${LSPR_FIX}/gl_view_issue.json"
		;;
	https://gitlab.test/api/v4/user)
		write_json "${LSPR_FIX}/gl_user.json"
		;;
	https://gitlab.test/api/v4/groups)
		write_json "${LSPR_FIX}/gl_groups.json"
		;;
	https://gitlab.test/api/v4/groups/*/merge_requests)
		write_json "${LSPR_FIX}/gl_group_mrs.json"
		;;
	https://gitlab.test/api/v4/groups/*/issues)
		write_json "${LSPR_FIX}/gl_group_issues.json"
		;;
	https://gitlab.test/api/v4/projects/*/merge_requests)
		write_json "${LSPR_FIX}/gl_project_mrs.json"
		;;
	https://gitlab.test/api/v4/projects/*/issues)
		write_json "${LSPR_FIX}/gl_group_issues.json"
		;;
	https://gitlab.test/api/graphql)
		if [[ -n "${data_file}" ]] && grep -q 'issues(' "${data_file}"
		then
			write_json "${LSPR_FIX}/gl_issue_graphql.json"
		else
			write_json "${LSPR_FIX}/gl_graphql.json"
		fi
		;;
	*)
		printf 'curl stub: unexpected URL %s\n' "${url}" >&2
		exit 97
		;;
esac
EOF
chmod +x "${STUB_BIN}/curl"

cat > "${STUB_BIN}/fzf" << 'EOF'
#!/bin/bash
set -euo pipefail
if [[ -n "${LSPR_FZF_EXIT:-}" ]]
then
	cat >/dev/null
	exit "${LSPR_FZF_EXIT}"
fi
head -n 1
EOF
chmod +x "${STUB_BIN}/fzf"

cat > "${STUB_BIN}/xdg-open" << 'EOF'
#!/bin/bash
set -euo pipefail
printf '%s\n' "${1}" >> "${LSPR_OPEN_LOG}"
EOF
chmod +x "${STUB_BIN}/xdg-open"

ok () {
	PASS=$((PASS + 1))
	printf 'ok  %s\n' "$*"
}

bad () {
	FAIL=$((FAIL + 1))
	printf 'FAIL %s\n' "$*" >&2
	if [[ -s "${ERR}" ]]
	then
		printf '     stderr: %s\n' "$(head -n 4 "${ERR}")" >&2
	fi
}

reset_logs () {
	: > "${CURL_LOG}"
	: > "${OPEN_LOG}"
	: > "${ERR}"
	: > "${OUT}"
}

run () {
	reset_logs
	set +e
	"$@" > "${OUT}" 2> "${ERR}"
	status=$?
	set -e
}

base_env () {
	env \
		FORGEJO_TOKEN=test-fj \
		GITHUB_TOKEN=test-gh \
		GITLAB_TOKEN=test-gl \
		LSPR_CURL="${STUB_BIN}/curl" \
		LSPR_FORGEJO_URL=https://forgejo.test \
		LSPR_GITLAB_URL=https://gitlab.test \
		LSPR_GITHUB_API=https://github.test \
		LSPR_CURL_LOG="${CURL_LOG}" \
		LSPR_OPEN_LOG="${OPEN_LOG}" \
		LSPR_FIX="${FIX}" \
		PATH="${STUB_BIN}:/usr/bin:/bin" \
		"$@"
}

# Help and license must work with no token and no curl.
run env -u FORGEJO_TOKEN -u GITHUB_TOKEN -u GITLAB_TOKEN PATH=/usr/bin:/bin "${SCRIPT}" --help
if [[ "${status}" -eq 0 ]] && grep -q -- '--fzf' "${OUT}" && grep -q 'OWNER/REPO' "${OUT}"
then
	ok "help"
else
	bad "help status=${status}"
fi

run env -u FORGEJO_TOKEN PATH=/usr/bin:/bin "${SCRIPT}" --license
if [[ "${status}" -eq 0 ]] && grep -q 'AGPL' "${OUT}"
then
	ok "license"
else
	bad "license"
fi

run env -u FORGEJO_TOKEN PATH=/usr/bin:/bin "${SCRIPT}" --version
if [[ "${status}" -eq 0 ]] && grep -q 'lspr 1.3.0' "${OUT}"
then
	ok "version"
else
	bad "version"
fi

run env -u FORGEJO_TOKEN PATH=/usr/bin:/bin "${SCRIPT}" --nope
if [[ "${status}" -eq 1 ]] && grep -q 'unknown option' "${ERR}"
then
	ok "unknown option"
else
	bad "unknown option status=${status}"
fi

run env -u FORGEJO_TOKEN PATH=/usr/bin:/bin "${SCRIPT}" --state
if [[ "${status}" -eq 1 ]] && grep -q 'requires a value' "${ERR}"
then
	ok "missing state value"
else
	bad "missing state value"
fi

run env -u FORGEJO_TOKEN PATH=/usr/bin:/bin "${SCRIPT}" --state weird
if [[ "${status}" -eq 1 ]] && grep -q 'state must be' "${ERR}"
then
	ok "bad state"
else
	bad "bad state"
fi

run env -u FORGEJO_TOKEN PATH=/usr/bin:/bin "${SCRIPT}" --limit -1
if [[ "${status}" -eq 1 ]]
then
	ok "bad limit"
else
	bad "bad limit status=${status}"
fi

run env -u FORGEJO_TOKEN PATH=/usr/bin:/bin "${SCRIPT}" 'not a repo'
if [[ "${status}" -eq 1 ]] && grep -q 'owner/name' "${ERR}"
then
	ok "bad repo"
else
	bad "bad repo"
fi

run env -u FORGEJO_TOKEN LSPR_CURL="${STUB_BIN}/curl" LSPR_CURL_LOG="${CURL_LOG}" PATH="${STUB_BIN}:/usr/bin:/bin" "${SCRIPT}"
if [[ "${status}" -eq 1 ]] && grep -q 'FORGEJO_TOKEN is not set' "${ERR}" && [[ ! -s "${CURL_LOG}" ]]
then
	ok "missing token before any request"
else
	bad "missing token status=${status}"
fi

run base_env "${SCRIPT}" --plain
plain_line="$(grep '^forgejo' "${OUT}" || true)"
expect="$(printf 'forgejo\tzach/libresite\t13\topen\tfeat site actions\talice\t2026-10-04T14:24:17Z\thttps://git.podbielniak.com/zach/libresite/pulls/13')"
lines="$(wc -l < "${OUT}" | tr -d '[:space:]')"
if [[ "${status}" -eq 0 && "${plain_line}" == "${expect}" && "${lines}" -eq 1 ]] && ! grep -q 'page=2' "${CURL_LOG}"
then
	ok "forgejo plain row"
else
	bad "forgejo plain row"
	printf '%s\n' "${plain_line}" >&2
fi

run base_env "${SCRIPT}" --state merged --plain
if [[ "${status}" -eq 0 ]] && grep -q $'old merged' "${OUT}" && ! grep -q 'old closed' "${OUT}"
then
	ok "merged filter"
else
	bad "merged filter"
fi

run base_env "${SCRIPT}" --state closed --plain
if [[ "${status}" -eq 0 ]] && grep -q 'old closed' "${OUT}" && grep -q 'old merged' "${OUT}"
then
	ok "closed includes merged"
else
	bad "closed includes merged"
fi

run base_env "${SCRIPT}" --repo widgets/demo --plain
lines="$(wc -l < "${OUT}" | tr -d '[:space:]')"
if [[ "${status}" -eq 0 && "${lines}" -eq 2 ]] \
	&& grep -q "$(printf 'forgejo\twidgets/demo\t2\topen\tmine pr')" "${OUT}" \
	&& grep -q '/repos/widgets/demo/pulls' "${CURL_LOG}" \
	&& ! grep -q '/api/v1/user$' "${CURL_LOG}" \
	&& ! grep -q 'issues/search' "${CURL_LOG}"
then
	ok "repo filter uses the pulls endpoint and the path hint"
else
	bad "repo filter"
fi

run base_env "${SCRIPT}" --repo widgets/demo --mine --plain
lines="$(wc -l < "${OUT}" | tr -d '[:space:]')"
if [[ "${status}" -eq 0 && "${lines}" -eq 1 ]] \
	&& grep -q 'mine pr' "${OUT}" \
	&& ! grep -q 'alice pr' "${OUT}"
then
	ok "mine filters the author"
else
	bad "mine filter"
fi

run base_env "${SCRIPT}" --github --plain
if [[ "${status}" -eq 0 ]] \
	&& grep -q "$(printf 'github\tLinear-Byte/LinearByte-Landing-Page\t3\topen\tlanding page\tbob')" "${OUT}" \
	&& grep -q 'org%3Awidgets' "${CURL_LOG}" \
	&& grep -q 'user%3Azach' "${CURL_LOG}" \
	&& ! grep -q 'EpicGames' "${CURL_LOG}"
then
	ok "github skips orgs you do not admin"
else
	bad "github org filter"
fi

run base_env "${SCRIPT}" --github --repo widgets/demo --plain
if [[ "${status}" -eq 0 ]] && grep -q 'repo%3Awidgets%2Fdemo' "${CURL_LOG}"
then
	ok "github repo query"
else
	bad "github repo query"
fi

run base_env "${SCRIPT}" --gitlab --plain
if [[ "${status}" -eq 0 ]] \
	&& grep -q "$(printf 'gitlab\timmutablue/immutablue\t173\topen\tfedora 44\tCreplav')" "${OUT}" \
	&& grep -q "$(printf 'gitlab\tzach/clawtilla\t2\topen\tclaw fix\tzach')" "${OUT}" \
	&& grep -q '/api/graphql' "${CURL_LOG}" \
	&& grep -q '/groups/9/merge_requests' "${CURL_LOG}"
then
	ok "gitlab group and personal rows"
else
	bad "gitlab rows"
	cat "${OUT}" >&2
fi

run base_env "${SCRIPT}" --gitlab --repo widgets/demo --plain
if [[ "${status}" -eq 0 ]] \
	&& grep -q "$(printf 'gitlab\twidgets/demo\t8\topen\tproject mr\tada')" "${OUT}" \
	&& grep -q 'projects/widgets%2Fdemo/merge_requests' "${CURL_LOG}" \
	&& ! grep -q '/api/graphql' "${CURL_LOG}" \
	&& ! grep -q '/api/v4/user' "${CURL_LOG}"
then
	ok "gitlab single project"
else
	bad "gitlab single project"
fi

run base_env "${SCRIPT}" --all --limit 1 --plain
first="$(head -n 1 "${OUT}")"
lines="$(wc -l < "${OUT}" | tr -d '[:space:]')"
expect="$(printf 'forgejo\tzach/libresite\t13\topen\tfeat site actions\talice\t2026-10-04T14:24:17Z\thttps://git.podbielniak.com/zach/libresite/pulls/13')"
if [[ "${status}" -eq 0 && "${lines}" -eq 1 && "${first}" == "${expect}" ]]
then
	ok "limit keeps the newest row"
else
	bad "limit"
fi

run base_env "${SCRIPT}" --fzf
opened="$(cat "${OPEN_LOG}")"
if [[ "${status}" -eq 0 && ! -s "${OUT}" && "${opened}" == "https://git.podbielniak.com/zach/libresite/pulls/13" ]]
then
	ok "fzf opens the selected URL"
else
	bad "fzf open status=${status}"
	printf 'open log: %s\n' "${opened}" >&2
fi

run env \
	FORGEJO_TOKEN=test-fj \
	LSPR_CURL="${STUB_BIN}/curl" \
	LSPR_FORGEJO_URL=https://forgejo.test \
	LSPR_CURL_LOG="${CURL_LOG}" \
	LSPR_OPEN_LOG="${OPEN_LOG}" \
	LSPR_FIX="${FIX}" \
	LSPR_FZF_EXIT=130 \
	PATH="${STUB_BIN}:/usr/bin:/bin" \
	"${SCRIPT}" --fzf
if [[ "${status}" -eq 130 ]] && [[ ! -s "${OPEN_LOG}" ]]
then
	ok "fzf cancel"
else
	bad "fzf cancel status=${status}"
fi

run env \
	FORGEJO_TOKEN=test-fj \
	LSPR_CURL="${STUB_BIN}/curl" \
	LSPR_FORGEJO_URL=https://forgejo.test \
	LSPR_CURL_LOG="${CURL_LOG}" \
	LSPR_FIX="${FIX}" \
	LSPR_STUB_401=1 \
	PATH="${STUB_BIN}:/usr/bin:/bin" \
	"${SCRIPT}" --plain
if [[ "${status}" -eq 1 ]] && grep -q 'FORGEJO_TOKEN' "${ERR}" && grep -q 'bad token' "${ERR}"
then
	ok "401 names the token variable"
else
	bad "401 status=${status}"
fi

run base_env "${SCRIPT}" --issues --plain
expect="$(printf 'forgejo\tzach/libresite\t7\topen\tbroken build\tada\t2026-10-01T00:00:00Z\thttps://git.podbielniak.com/zach/libresite/issues/7')"
plain_line="$(grep '^forgejo' "${OUT}" || true)"
if [[ "${status}" -eq 0 && "${plain_line}" == "${expect}" ]] \
	&& grep -q 'type=issues' "${CURL_LOG}" \
	&& ! grep -q 'type=pulls' "${CURL_LOG}"
then
	ok "issues list"
else
	bad "issues list"
	printf '%s\n' "${plain_line}" >&2
fi

run base_env "${SCRIPT}" --issue --plain
if [[ "${status}" -eq 0 ]] && grep -q 'broken build' "${OUT}" && grep -q 'type=issues' "${CURL_LOG}"
then
	ok "issue is an alias"
else
	bad "issue alias"
fi

run base_env "${SCRIPT}" -iR widgets/demo --plain
if [[ "${status}" -eq 0 ]] \
	&& grep -q "$(printf 'forgejo\twidgets/demo\t9\topen\trepo issue')" "${OUT}" \
	&& grep -q '/repos/widgets/demo/issues?type=issues' "${CURL_LOG}" \
	&& ! grep -q '/pulls' "${CURL_LOG}"
then
	ok "clustered -iR"
else
	bad "clustered -iR"
fi

run base_env "${SCRIPT}" -iRwidgets/demo --plain
if [[ "${status}" -eq 0 ]] && grep -q '/repos/widgets/demo/issues?type=issues' "${CURL_LOG}"
then
	ok "attached -iR value"
else
	bad "attached -iR value"
fi

run base_env "${SCRIPT}" -R widgets/demo --plain
if [[ "${status}" -eq 0 ]] \
	&& grep -q '/repos/widgets/demo/pulls' "${CURL_LOG}" \
	&& ! grep -q 'type=issues' "${CURL_LOG}"
then
	ok "-R selects a repo"
else
	bad "-R selects a repo"
fi

run env -u FORGEJO_TOKEN PATH=/usr/bin:/bin "${SCRIPT}" -iR
if [[ "${status}" -eq 1 ]] && grep -q -- '-R requires a value' "${ERR}"
then
	ok "clustered -R needs a value"
else
	bad "clustered -R needs a value"
fi

run env -u FORGEJO_TOKEN PATH=/usr/bin:/bin "${SCRIPT}" -iZ
if [[ "${status}" -eq 1 ]] && grep -q 'unknown option: -Z' "${ERR}"
then
	ok "unknown flag in a cluster"
else
	bad "unknown flag in a cluster"
fi

run base_env "${SCRIPT}" --issues --repo widgets/demo --plain
if [[ "${status}" -eq 0 ]] \
	&& grep -q "$(printf 'forgejo\twidgets/demo\t9\topen\trepo issue')" "${OUT}" \
	&& grep -q '/repos/widgets/demo/issues?type=issues' "${CURL_LOG}" \
	&& ! grep -q '/pulls' "${CURL_LOG}"
then
	ok "repo issues endpoint"
else
	bad "repo issues endpoint"
fi

run env -u FORGEJO_TOKEN PATH=/usr/bin:/bin "${SCRIPT}" --issues --state merged
if [[ "${status}" -eq 1 ]] && grep -q 'no merged state' "${ERR}"
then
	ok "merged rejected for issues"
else
	bad "merged rejected for issues"
fi

run base_env "${SCRIPT}" --github --issues --plain
if [[ "${status}" -eq 0 ]] \
	&& grep -q "$(printf 'github\twidgets/demo\t4\topen\tgithub issue\tada')" "${OUT}" \
	&& grep -q 'is%3Aissue' "${CURL_LOG}" \
	&& ! grep -q 'is%3Apr' "${CURL_LOG}"
then
	ok "github issues"
else
	bad "github issues"
fi

run base_env "${SCRIPT}" --gitlab --issues --plain
if [[ "${status}" -eq 0 ]] \
	&& grep -q "$(printf 'gitlab\timmutablue/immutablue\t11\topen\tgroup issue\tada')" "${OUT}" \
	&& grep -q "$(printf 'gitlab\tzach/clawtilla\t6\topen\tpersonal issue\tzach')" "${OUT}" \
	&& grep -q '/groups/9/issues' "${CURL_LOG}" \
	&& ! grep -q 'merge_requests' "${CURL_LOG}"
then
	ok "gitlab issues"
else
	bad "gitlab issues"
	cat "${OUT}" >&2
fi

# Pretty output needs a terminal. The pty closes when lspr exits.
python3 - "${SCRIPT}" "${STUB_BIN}" "${FIX}" "${CURL_LOG}" "${OPEN_LOG}" << 'PY'
import fcntl
import os
import pty
import re
import struct
import sys
import termios

script, stub_bin, fix, curl_log, open_log = sys.argv[1:6]
master, slave = pty.openpty()
# 80 columns. lspr must stay inside that, including the spare last column.
fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", 24, 80, 0, 0))
# A minimal environment, so a parent NO_COLOR or BASH_ENV cannot disable color.
env = {
    "FORGEJO_TOKEN": "test-fj",
    "LSPR_CURL": os.path.join(stub_bin, "curl"),
    "LSPR_FORGEJO_URL": "https://forgejo.test",
    "LSPR_CURL_LOG": curl_log,
    "LSPR_OPEN_LOG": open_log,
    "LSPR_FIX": fix,
    "PATH": stub_bin + ":/usr/bin:/bin",
    "HOME": os.environ.get("HOME", "/tmp"),
    "TERM": "xterm-256color",
    "LANG": os.environ.get("LANG", "C.UTF-8"),
}
pid = os.fork()
if pid == 0:
    os.setsid()
    os.dup2(slave, 1)
    os.dup2(slave, 2)
    os.close(master)
    os.close(slave)
    os.execve(script, [script], env)
os.close(slave)
chunks = []
while True:
    try:
        data = os.read(master, 4096)
    except OSError:
        break
    if not data:
        break
    chunks.append(data)
os.close(master)
_, status = os.waitpid(pid, 0)
text = b"".join(chunks).decode("utf-8", "replace")
code = os.waitstatus_to_exitcode(status)
# The title is truncated to the pty width, so match the repo and a color, not the full title.
visible = re.sub(r"\033\[[0-9;]*m", "", text)
wide = [line for line in visible.splitlines() if len(line) >= 80]
if code != 0 or "FORGE" not in text or "libresite" not in text or "\033[32m" not in text or "pull request" not in text or wide:
    sys.stderr.write(f"pretty status={code} wide={wide!r}\n{text}\n")
    sys.exit(1)
PY
ok "pretty table on a tty"

run base_env "${SCRIPT}" --review --plain
if [[ "${status}" -eq 0 ]] \
	&& grep -q "$(printf 'forgejo\twidgets/demo\t8\topen\tplease review')" "${OUT}" \
	&& grep -q "$(printf 'forgejo\tother/repo\t2\topen\tother review')" "${OUT}" \
	&& grep -q 'review_requested=true' "${CURL_LOG}" \
	&& ! grep -q '/user/orgs' "${CURL_LOG}" \
	&& ! grep -q '/api/v1/user' "${CURL_LOG}"
then
	ok "review requests are not the owned-project set"
else
	bad "forgejo review"
	printf '%s\n' "${OUT}" >&2
fi

run base_env "${SCRIPT}" --review -R widgets/demo --plain
if [[ "${status}" -eq 0 ]] \
	&& grep -q 'please review' "${OUT}" \
	&& ! grep -q 'other review' "${OUT}" \
	&& grep -q 'owner=widgets' "${CURL_LOG}" \
	&& grep -q 'review_requested=true' "${CURL_LOG}"
then
	ok "review can be limited to a repo"
else
	bad "review repo filter"
fi

run base_env "${SCRIPT}" --review --github --plain
if [[ "${status}" -eq 0 ]] \
	&& grep -q "$(printf 'github\tother/repo\t3\topen\tneeds you')" "${OUT}" \
	&& grep -q 'review-requested' "${CURL_LOG}" \
	&& ! grep -q 'memberships/orgs' "${CURL_LOG}" \
	&& ! grep -q 'org%3Awidgets' "${CURL_LOG}"
then
	ok "github review-requested"
else
	bad "github review"
fi

run base_env "${SCRIPT}" --review --gitlab --plain
if [[ "${status}" -eq 0 ]] \
	&& grep -q "$(printf 'gitlab\tother/repo\t4\topen\tplease review mr')" "${OUT}" \
	&& grep -q '/api/v4/merge_requests?' "${CURL_LOG}" \
	&& grep -q 'reviewer_username=zach' "${CURL_LOG}" \
	&& grep -q 'scope=all' "${CURL_LOG}" \
	&& ! grep -q '/groups/' "${CURL_LOG}"
then
	ok "gitlab review requests"
else
	bad "gitlab review"
	cat "${CURL_LOG}" >&2
fi

run base_env "${SCRIPT}" --review --mine --plain
if [[ "${status}" -eq 0 ]] \
	&& grep -q 'review_requested=true' "${CURL_LOG}" \
	&& grep -q 'created_by=zach' "${CURL_LOG}"
then
	ok "review composes with --mine"
else
	bad "review --mine"
fi

run env -u FORGEJO_TOKEN PATH=/usr/bin:/bin "${SCRIPT}" --review --issues
if [[ "${status}" -eq 1 ]] && grep -q 'reviews are pull requests' "${ERR}"
then
	ok "review rejects issues"
else
	bad "review rejects issues"
fi

run base_env "${SCRIPT}" --view widgets/demo#9 --plain
if [[ "${status}" -eq 0 ]] \
	&& grep -q 'repo issue' "${OUT}" \
	&& grep -q 'the build is red' "${OUT}" \
	&& grep -q 'second paragraph' "${OUT}" \
	&& grep -q 'labels: bug' "${OUT}" \
	&& grep -q 'assignees: zach' "${OUT}" \
	&& grep -q 'I can reproduce this.' "${OUT}" \
	&& grep -q 'widgets/demo#9' "${OUT}" \
	&& ! grep -q $'\033' "${OUT}" \
	&& grep -q '/repos/widgets/demo/issues/9' "${CURL_LOG}" \
	&& ! grep -q '/pulls/' "${CURL_LOG}"
then
	ok "view an issue"
else
	bad "view an issue"
	printf '%s\n' "${OUT}" >&2
fi

run base_env "${SCRIPT}" --view 'https://forgejo.test/widgets/demo/issues/9' --plain
if [[ "${status}" -eq 0 ]] && grep -q '/repos/widgets/demo/issues/9' "${CURL_LOG}"
then
	ok "view a forgejo URL"
else
	bad "view a forgejo URL"
fi

run base_env "${SCRIPT}" --view 'https://github.com/widgets/demo/pull/3' --plain
if [[ "${status}" -eq 0 ]] \
	&& grep -q 'feature -> main' "${OUT}" \
	&& grep -q 'LGTM' "${OUT}" \
	&& grep -q 'approved' "${OUT}" \
	&& grep -q 'main.c:10' "${OUT}" \
	&& grep -q 'nit here' "${OUT}" \
	&& grep -q '/repos/widgets/demo/pulls/3' "${CURL_LOG}"
then
	ok "view a github pull request"
else
	bad "view a github pull request"
	printf '%s\n' "${OUT}" >&2
fi

run base_env "${SCRIPT}" --view 'https://gitlab.test/widgets/demo/-/merge_requests/4' --plain
if [[ "${status}" -eq 0 ]] \
	&& grep -q 'the change itself' "${OUT}" \
	&& grep -q 'ship it' "${OUT}" \
	&& grep -q 'feature -> main' "${OUT}" \
	&& ! grep -q 'assigned to zach' "${OUT}" \
	&& grep -q '/projects/widgets%2Fdemo/merge_requests/4' "${CURL_LOG}"
then
	ok "view a gitlab merge request"
else
	bad "view a gitlab merge request"
	printf '%s\n' "${OUT}" >&2
	cat "${CURL_LOG}" >&2
fi

run base_env "${SCRIPT}" --view 'widgets/demo!4' --plain
if [[ "${status}" -eq 0 ]] && grep -q '/projects/widgets%2Fdemo/merge_requests/4' "${CURL_LOG}"
then
	ok "gitlab ! ref"
else
	bad "gitlab ! ref"
fi

run env -u FORGEJO_TOKEN PATH=/usr/bin:/bin "${SCRIPT}" --view
if [[ "${status}" -eq 1 ]] && grep -q 'requires a value' "${ERR}"
then
	ok "view needs a value"
else
	bad "view needs a value"
fi

run env -u FORGEJO_TOKEN PATH=/usr/bin:/bin "${SCRIPT}" --all --view widgets/demo#9
if [[ "${status}" -eq 1 ]] && grep -q 'pass one forge' "${ERR}"
then
	ok "view with every forge needs a URL"
else
	bad "ambiguous view"
fi

printf '%s passed, %s failed\n' "${PASS}" "${FAIL}"
if [[ "${FAIL}" -ne 0 ]]
then
	exit 1
fi
