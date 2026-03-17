# Mac migration checklist

Things to copy over when migrating to a new machine.

## Shell & dotfiles
- `~/.ssh/` — keys, config, known_hosts, authorized_keys
- `~/.gnupg/` — GPG keys and trust store
- `~/.netrc` — plaintext credentials
- `~/.zsh_history`
- `~/.gitconfig` (and `~/.gitconfig.local` if secrets are split out)

## Credentials & dev tools
- `~/.aws/` — credentials, config, SSO cache
- `~/.kube/config` — Kubernetes contexts and certs
- `~/.docker/config.json` — registry auth tokens
- `~/.config/gcloud/` — Google Cloud credentials
- `~/.config/gh/` — GitHub CLI auth token
- `~/.npmrc` — npm auth tokens
- `~/.pypirc` — PyPI credentials
- `~/.config/rclone/` — cloud storage credentials

## Email — Apple Mail
- `~/Library/Mail/`
- `~/Library/Containers/com.apple.mail/Data/`
- `~/Library/Preferences/com.apple.mail.plist`

## Browsers
- `~/Library/Application Support/Firefox/Profiles/<profile>/` — bookmarks, logins.json, key4.db
- `~/Library/Application Support/Google/Chrome/Default/`
- `~/Library/Application Support/Arc/User Data/Default/`

## Terminal
- `~/Library/Application Support/iTerm2/`
- `~/Library/Preferences/com.googlecode.iterm2.plist`

## Calendar, Contacts, Messages, Notes
- `~/Library/Calendars/`
- `~/Library/Contacts/`
- `~/Library/Messages/`
- `~/Library/Group Containers/group.com.apple.notes/`

## Media
- `~/Pictures/Photos Library.photoslibrary`
- `~/Library/Fonts/`

## macOS system
- `~/Library/KeyBindings/DefaultKeyBinding.dict`

## Syncthing
- `~/Library/Application Support/Syncthing/config.xml` — folder definitions and device pairings
- `~/Library/Application Support/Syncthing/cert.pem` + `key.pem` — device identity (preserves Device ID, avoids re-approvals on peers)
- Check `config.xml` for the actual synced folder paths
