# mailmasquerade

A small daemon that proxies e-mail between one "target" mailbox and a set of
whitelisted correspondents, so the target's real address never has to be
given out directly.

- Mail arriving **from a whitelisted address** is forwarded to the target,
  rewritten so it looks like it came from the mailbox mailmasquerade itself
  logs into.
- Mail arriving **from the target**, replying to something that came in
  through mailmasquerade, is sent back to whoever originally wrote in
  (mailmasquerade keeps a small on-disk database mapping `Message-ID`s to
  senders for this).
- If the target has no configured `defaultReplyTo`, and no match is found in
  that database, the reply falls back to `defaultReplyTo`.
- A whitelisted sender can send a mail with the subject `PING` (case- and
  whitespace-insensitive) to get an immediate `PONG` reply back, as a
  liveness check that doesn't touch the target mailbox at all.
- Anything else (mail from neither the target nor the whitelist) is silently
  dropped.

Mail is picked up over IMAP (with `IDLE`, polled every 30 seconds) and sent
back out over SMTP.

## Building

```
cabal build
```

`cabal.project` temporarily pins the `HaskellNet` dependency to a specific
git commit rather than the latest Hackage release — the released version's
`fetch` implementation parses the whole message body (including
attachments) through a `String`-based parser, which causes a large memory
spike on mail with big or numerous attachments. See the comment in
`cabal.project` for details; it should be dropped once a release with the
fix ships.

## Running

```
mailmasquerade -c /path/to/config.json
```

### Command-line flags

| Flag | Meaning |
| --- | --- |
| `-c`, `--config` | Path to the JSON config file (required). |
| `-v`, `--verbose` | Log at `INFO` level: routing decisions, connect/login/send events. |
| `-d`, `--debug` | Log at `DEBUG` level: everything `-v` shows, plus full raw MIME headers of every message handled. This is sensitive (addresses, subjects, DKIM signatures, ...) — only turn it on when actually debugging. |
| `-s`, `--stdout` | Write logs to stdout instead of syslog. |

With neither `-v` nor `-d`, only `WARNING` and above is logged (recoverable
issues like a dropped IMAP connection or a failed reply-database write) plus
`ERROR` (parse failures, send failures, anything else unexpected).

mailmasquerade keeps its reply-routing state in `replydb.bin` in the
working directory (the Message-ID → sender-address database used to route
the target's replies back to the right person), plus a `replydb.bin.bak`
backup written before each update.

Both are excluded from git via `.gitignore`, along with the conventional
config file name `mailmasquerade.json` and `whitelist.txt`, so real
credentials and runtime state never end up committed by accident.

An OpenRC init script is provided in `openrc/mailmasquerade`.

## Configuration

The config file is JSON, matching this shape:

```json
{
    "imapServer": "imap.example.com",
    "smtpServer": "smtp.example.com",
    "username": "bot@example.com",
    "password": "hunter2",
    "target": "real-person@example.com",
    "whitelist": ["someone@example.com", "someone-else@example.com"],
    "defaultReplyTo": ["fallback@example.com"]
}
```

| Field | Meaning |
| --- | --- |
| `imapServer` | IMAP server to log into (over TLS) to receive mail. |
| `smtpServer` | SMTP server to send mail through (over TLS). |
| `username` | The mailbox mailmasquerade logs into and sends mail as. |
| `password` | Password for `username`, used for both IMAP and SMTP auth. |
| `target` | The address being masqueraded — the person whose real address the whitelist never sees directly. |
| `whitelist` | Addresses allowed to reach `target` through mailmasquerade (and to use the `PING` health check). |
| `defaultReplyTo` | Address(es) to use when the target replies to something mailmasquerade can't find an original sender for. |

## Logging

Log lines are namespaced by subsystem so they can be told apart (and, with a
syslog-aware setup, filtered/routed) at a glance:

- `mailmasquerade.config` — startup and config loading.
- `mailmasquerade.imap` — connecting, logging in, IDLE/poll cycle.
- `mailmasquerade.smtp` — outgoing mail.
- `mailmasquerade.mail` — routing decisions for each incoming message
  (prefixed with that message's `Message-ID` where available).
- `mailmasquerade.mail.ping` — the `PING`/`PONG` health check.
- `mailmasquerade.replydb` — the reply-address database.

## Testing

```
cabal test
```

runs the hspec suite in `MainSpec.hs`, which covers header rewriting for
forwarded and replied-to mail.
