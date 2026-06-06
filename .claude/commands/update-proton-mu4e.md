---
name: update-proton-mu4e
description: >
  Walk through recovering Proton Mail + mu4e after Proton logs you out (happens
  roughly every 30 days). Re-authenticates Proton Bridge, refreshes the bridge
  password and TLS cert in the mbsync/msmtp configs, then resets the local mbsync
  cache to clear the UIDVALIDITY-change errors and re-syncs/re-indexes mail.
  Trigger when: "proton logged me out", "re-auth proton bridge", "mu4e stopped
  syncing", "UIDVALIDITY change", "reset proton mu4e", "mbsync proton broken",
  "can't recover from UIDVALIDITY", or the 30-day Proton re-login dance.
---

# Skill: Recover Proton Mail + mu4e After a Bridge Re-login

Proton logs the user out of Proton Mail Bridge roughly every 30 days. After signing
back in, mu4e stops syncing because Bridge has reset each folder's `UIDVALIDITY`,
and (occasionally) the bridge password or TLS cert have changed too. This skill
walks through the full recovery with confirmation gates at every manual step.

## Background — how this mailbox is wired

This is a **Proton Mail Bridge** setup running on `127.0.0.1`:

- **Receiving:** `mbsync` (isync) — config `~/.config/isync/mbsyncrc`, IMAP `127.0.0.1:1143`,
  maildir `~/.local/share/mail/proton/`.
- **Sending:** `msmtp` — config `~/.config/msmtp/config`, SMTP `127.0.0.1:1025`.
- **Index:** `mu` — Xapian DB at `~/.cache/mu/xapian`, queried live by mu4e.
- The mbsync channel is **bidirectional** (`Create Both`, `Expunge Both`). This is the
  single most important fact for the cache reset — see the ⚠️ warning in Phase 3.

The credentials/cert are **not** in any Emacs/Doom file. They live in:

| What | File | Field |
|------|------|-------|
| Bridge password (receiving) | `~/.config/isync/mbsyncrc` | `Pass <…>` line |
| Bridge password (sending) | `~/.config/msmtp/config` | `password <…>` line |
| Bridge TLS cert (receiving) | `~/.config/isync/bridge-cert.pem` | whole file (+ `key.pem` companion) |

`msmtp` has `tls_certcheck off`, so the cert is only used by `mbsync`. None of these
files are tracked in the dotfiles repo (not stowed), so edits don't show in `git status`.

---

## Procedure

Work through the phases in order. **Stop at every "🛑 CONFIRM" gate** and wait for the
user to confirm before continuing. Run shell steps for the user; pause for anything
that happens in the Bridge GUI.

### Phase 0 — Re-authenticate Proton Bridge (user, in the GUI)

Ask the user to:
1. Open the **Proton Mail Bridge** app and sign back into their Proton account.
2. Open **Settings → IMAP/SMTP** and confirm the ports are still **IMAP 1143 / SMTP 1025**.
3. Open the account's **Mailbox details / configuration** and have the **bridge password**
   visible (the per-Bridge password, *not* the Proton account password).

🛑 **CONFIRM:** "Are you signed back into Bridge, with the bridge password visible?"

### Phase 1 — Quick test first (don't fix what isn't broken)

Often only the cache (Phase 3) needs work; the password/cert survive a routine re-login.
Run a test sync to see which failures actually occur:

```bash
mbsync -c ~/.config/isync/mbsyncrc proton 2>&1; echo "exit: $?"
```

Interpret the output:
- **`AUTHENTICATIONFAILED` / login errors** → password changed → do **Phase 2a**.
- **TLS / certificate errors** (`unable to get local issuer`, handshake failure) → cert
  changed → do **Phase 2b**.
- **`Unable to recover from UIDVALIDITY change` / `UIDVALIDITY genuinely changed`** →
  the usual case → go to **Phase 3**.
- **Clean exit, mail flows** → nothing to do; skip to **Phase 4** to reindex if needed.

### Phase 2a — Refresh the bridge password (only if auth failed)

The same bridge password goes in **both** files. Show the current (redacted) lines so
the user sees exactly what to change:

```bash
grep -nE '^Pass'      ~/.config/isync/mbsyncrc | sed -E 's/(Pass[[:space:]]+).*/\1<CURRENT>/'
grep -nE '^password'  ~/.config/msmtp/config   | sed -E 's/(password[[:space:]]+).*/\1<CURRENT>/'
```

Ask the user to paste the new bridge password (and either let Claude apply the edits with
the Edit tool, or edit the two files themselves):
- `~/.config/isync/mbsyncrc` → `Pass <new>`
- `~/.config/msmtp/config` → `password <new>`

🛑 **CONFIRM:** "Bridge password updated in **both** `mbsyncrc` and `msmtp/config`?"

### Phase 2b — Re-export the TLS cert (only if TLS failed, or after a Bridge reinstall)

In the Bridge GUI: **Settings → Advanced → Export TLS certificates**. Then save them:
- Certificate → **`~/.config/isync/bridge-cert.pem`** (this is the one `mbsync`'s
  `CertificateFile` points to — required).
- Private key → `~/.config/isync/key.pem` (companion; mbsync doesn't reference it, fine to
  overwrite alongside).

Verify the cert landed and looks sane (don't print the key):

```bash
ls -l ~/.config/isync/bridge-cert.pem ~/.config/isync/key.pem
openssl x509 -in ~/.config/isync/bridge-cert.pem -noout -subject -enddate 2>&1
```

🛑 **CONFIRM:** "New `bridge-cert.pem` saved and `openssl` shows a valid future expiry?"

After 2a/2b, re-run the Phase 1 test sync. If it now only complains about UIDVALIDITY,
continue to Phase 3.

### Phase 3 — Reset the local mbsync cache (the UIDVALIDITY fix)

> ⚠️ **Why the whole local cache, not just the state files.** The channel is
> `Create Both` + `Expunge Both` (bidirectional). If you delete **only** the
> `.mbsyncstate`/`.uidvalidity` files, mbsync re-reads every local message as "new, not
> yet on the server" and **pushes duplicates up to Proton**. If you delete **only** the
> message files but keep the state, mbsync reads them as local deletions and
> **expunges them from Proton**. The safe reset is to remove messages **and** state
> **together** so there's nothing to push and nothing read as a deletion — then pull fresh.
>
> This relies on Proton being the source of truth (no local-only mail). Confirm that
> assumption holds before wiping.

🛑 **CONFIRM:** "OK to reset the local cache? Everything is on the Proton side — no
local-only mail/drafts to lose?"

Then back up (instant rename, reversible — never `rm` the live cache), recreate empty,
and re-sync:

```bash
set -euo pipefail
ts=$(date +%Y%m%d-%H%M%S)
src=~/.local/share/mail/proton
bak=~/.local/share/mail/proton.bak.$ts
mv "$src" "$bak"
mkdir -p "$src"
echo "backed up -> $bak"

mbsync -c ~/.config/isync/mbsyncrc proton 2>&1; echo "exit: $?"
```

Expect a clean run: `Far: +0 *0 #0 -0` (nothing changed on Proton's side) and
`Near: +N` (N messages pulled down), with no UIDVALIDITY errors. Report N to the user.

### Phase 4 — Reindex mu and refresh mu4e

The CLI sync bypassed mu4e, so rebuild the index:

```bash
mu index 2>&1 | tail -3; echo "exit: $?"
```

🛑 **CONFIRM (user, in Emacs):** If mu4e is open, refresh it with **`U`** in the mu4e main
view (or `M-x mu4e-update-mail-and-index`), or restart mu4e — the Xapian DB changed under
a running session. Confirm mail is now readable.

### Phase 5 — Clean up

Once the user confirms mail reads correctly, remove the backup created in Phase 3:

```bash
rm -rf ~/.local/share/mail/proton.bak.<timestamp>   # use the exact path printed earlier
```

---

## Verification checklist

- [ ] Phase 1 test sync no longer prints UIDVALIDITY / auth / TLS errors.
- [ ] Phase 3 sync showed `Far: +0 *0 #0 -0` and `Near: +N` with `exit: 0`.
- [ ] `mu index` finished with `exit: 0` and indexed ~N messages.
- [ ] mu4e (`U`) shows current mail.
- [ ] Backup dir removed.

## Notes & gotchas

- **Going forward**, routine refreshes should be done from inside mu4e with **`U`**, which
  runs the configured `mbsync` get-mail command and reindexes in one step.
- **Cosmetic warning:** `SSLType is deprecated. Use TLSType instead.` — to silence it,
  rename `SSLType STARTTLS` → `TLSType STARTTLS` in `~/.config/isync/mbsyncrc`. Optional.
- The bridge password and TLS cert usually **survive** a routine 30-day re-login; only
  the UIDVALIDITY reset (Phase 3) is typically needed. Phases 2a/2b are for when Bridge
  was reinstalled or its data cleared. Always let the Phase 1 test decide.
- Per repo policy, never run destructive DB/`psql` commands here; this skill only touches
  the local maildir cache (safely backed up first) and the mu index (rebuildable).
