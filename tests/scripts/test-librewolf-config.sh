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
set -euo pipefail
repo_dir="$(CDPATH='' cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd)"
script="${repo_dir}/bin/scripts/librewolf-config"
fixture="$(mktemp -d)"
trap 'rm -rf -- "${fixture}"' EXIT
mkdir -p "${fixture}/root/random profile/chrome" "${fixture}/root/legacy"
profile="${fixture}/root/random profile"
printf '%s\n' '// unrelated preference' > "${profile}/prefs.js"
printf '%s\n' 'user_pref("example.setting", 42);' > "${profile}/user.js"
printf '%s\n' 'original css' > "${profile}/chrome/userChrome.css"
cat > "${fixture}/root/profiles.ini" <<'INI'
[Profile0]
Name=default-default
IsRelative=1
Path=random profile
[InstallTEST]
Default=random profile
[Profile1]
Name=default
IsRelative=1
Path=legacy
Default=1
INI

# Install defaults win over the old profile default, including paths with spaces.
[[ "$("${script}" profile --root "${fixture}/root")" == "${profile}" ]]
"${script}" --help >/dev/null
"${script}" --license >/dev/null
"${script}" plan --root "${fixture}/root" >/dev/null
[[ ! -e "${profile}/user.js.~1~" ]]
"${script}" apply --root "${fixture}/root" >/dev/null
cmp "${repo_dir}/share/librewolf/chrome/userChrome.css" "${profile}/chrome/userChrome.css"
rg -q 'example.setting' "${profile}/user.js"
rg -q 'stylesheets", true' "${profile}/user.js"
rg -q 'original css' "${profile}/chrome/userChrome.css.~1~"
"${script}" apply --profile "${profile}" >/dev/null
[[ ! -e "${profile}/user.js.~2~" ]]
printf '%s\n' 'PASS: install default, spaces, preview, backups, preference preservation, idempotence'

# A dangling lock is still a lock; failure must preserve the installed files.
ln -s '127.0.0.1:+999999' "${profile}/lock"
if "${script}" apply --profile "${profile}" 2>/dev/null; then exit 1; fi
unlink "${profile}/lock"
printf '\n[InstallOTHER]\nDefault=legacy\n' >> "${fixture}/root/profiles.ini"
if "${script}" profile --root "${fixture}/root" 2>/dev/null; then exit 1; fi
"${script}" profile --profile "${profile}" >/dev/null
if "${script}" profile --profile 2>/dev/null; then exit 1; fi
if "${script}" save-export bitwarden /does/not/exist 2>/dev/null; then exit 1; fi
printf '%s\n' 'PASS: locks, conflicting install defaults, explicit override, missing argument, rejected vault export'

# Legacy-only discovery and absolute profiles also work.
printf '[Profile0]\nPath=%s\nIsRelative=0\nDefault=1\n' "${profile}" > "${fixture}/root/profiles.ini"
[[ "$("${script}" profile --root "${fixture}/root")" == "${profile}" ]]
printf '%s\n' '// BEGIN dotfiles librewolf' >> "${profile}/user.js"
cp "${profile}/user.js" "${fixture}/before"
if "${script}" apply --profile "${profile}" 2>/dev/null; then exit 1; fi
cmp "${fixture}/before" "${profile}/user.js"
printf '%s\n' 'PASS: absolute profile and malformed-block preservation'
