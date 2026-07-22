# NT Sentinel (`network-monitor`)

Command-line network monitoring tool for macOS and Linux. NT Sentinel shows live bandwidth, TCP connections, outbound traffic flows, DNS/traceroute tools, and an interactive ASCII menu.

**Project location:** `~/network-monitor`

## Build

```bash
cd ~/network-monitor
cabal build
```

Run from the project directory:

```bash
cabal run network-monitor -- <command> [options]
```

Or install globally:

```bash
cabal install network-monitor --installdir=~/.local/bin
network-monitor menu
```

## Interactive menu

Launch the full menu (recommended starting point):

```bash
cabal run network-monitor -- menu
```

| # | Feature |
|---|---------|
| 1 | Interface Statistics |
| 2 | Live Bandwidth Monitor (Watch) |
| 3 | TCP Connections |
| 4 | Visual NetView |
| 5 | Traffic Flow (Emit Monitor) |
| 6 | Network Dashboard |
| 7 | Ping Host |
| 8 | Port Check |
| 9 | Top Remote Hosts |
| 10 | Configure Session Options |
| 11 | Help |
| 12 | Mission Control |
| 13 | App Traffic Summary |
| 14 | Traceroute |
| 15 | DNS Trace |
| 16 | Listening Ports |
| 17 | Network Intel |
| 18 | Multi-Ping Board |
| 19 | Export Session Report |
| 0 | Exit |

Session defaults are stored at `~/.config/new-tower/session.conf`. Session logs and reports go to:

- `~/.config/new-tower/sessions.log`
- `~/.config/new-tower/reports/`

## Commands

| Command | Alias | Description |
|---------|-------|-------------|
| `interfaces` | `if` | Interface byte/packet counters |
| `watch` | `w` | Live bandwidth with spark bars |
| `connections` | `conn` | Active TCP connections |
| `dashboard` | `dash` | One-screen status overview |
| `netview` | `nv` | Live visual topology map |
| `flow` | `emit` | Live outbound traffic / emit monitor |
| `trace` | | Traceroute to a host |
| `dns` | | DNS record trace |
| `listen` | | Local listening ports + processes |
| `apps` | | Per-app traffic summary |
| `mission` | | All-in-one mission control panel |
| `intel` | | Public IP, gateway, Wi-Fi, VPN snapshot |
| `report` | | Export session report file |
| `mping` | | Multi-target ping board |
| `ping` | | Ping latency stats |
| `top` | | Top remote hosts by connection count |
| `port` | | TCP port reachability check |
| `menu` | | Interactive menu |

## Common examples

```bash
# Live Wi-Fi bandwidth (Ctrl-C to stop)
cabal run network-monitor -- watch -I en0

# See what you are sending and to whom
cabal run network-monitor -- flow

# Mission control + intel snapshot
cabal run network-monitor -- mission
cabal run network-monitor -- intel

# Diagnostics
cabal run network-monitor -- trace -H 8.8.8.8
cabal run network-monitor -- dns -H google.com
cabal run network-monitor -- listen

# Multi-ping favorites
cabal run network-monitor -- mping -F 8.8.8.8 -F 1.1.1.1

# Export a report
cabal run network-monitor -- report
```

## Command-line options

| Flag | Default | Meaning |
|------|---------|---------|
| `-i, --interval SECS` | `1.0` | Refresh interval (watch/flow/mission) |
| `-c, --count N` | `0` | Refresh count; `0` = until Ctrl-C |
| `-I, --interface IFACE` | (all) | Limit to interface(s), e.g. `en0` |
| `-s, --state STATE` | `ESTABLISHED` | Connection state filter |
| `-l, --limit N` | `50` | Max rows to display |
| `-H, --host HOST` | `8.8.8.8` | Target host |
| `-p, --port PORT` | `443` | Target port (port check) |
| `--pings N` | `4` | Ping packet count |
| `-F, --favorite HOST` | | Favorite host (repeatable, multi-ping) |
| `--block HOST` | | Blocklisted host prefix (repeatable) |
| `--emit-alert BPS` | `524288` | Emit rate alert threshold (bytes/sec) |
| `--log` | off | Append events to sessions log |
| `-h, --help` | | Show help |

## Session configuration

Edit via menu option **10**, or edit `~/.config/new-tower/session.conf` directly:

| Key | Default | Purpose |
|-----|---------|---------|
| `interface` | `en0` | Default interface filter |
| `interval` | `1.0` | Watch/flow refresh seconds |
| `count` | `0` | Auto-stop after N refreshes |
| `state` | `ESTABLISHED` | Connection state filter |
| `limit` | `50` | Max connection rows |
| `favorites` | `8.8.8.8,1.1.1.1` | Multi-ping board targets |
| `blocklist` | (empty) | Alert when flows match |
| `emit_alert` | `524288` | High emit rate alert (B/s) |
| `logging` | `true` | Write session events to log |

## Interface names

On macOS:

```bash
ifconfig -l
```

| Name | Typical use |
|------|-------------|
| `en0` | Wi-Fi |
| `lo0` | Loopback |

## Documentation PDF

Generate the command reference PDF (also copied to the Desktop):

```bash
python3 -m venv .venv-docs
.venv-docs/bin/pip install -r requirements-docs.txt
.venv-docs/bin/python generate-docs-pdf.py
```

Output:

- `docs/network-monitor-commands.pdf`
- `~/Desktop/network-monitor-commands.pdf`

## Requirements

- GHC / Cabal (Haskell)
- macOS or Linux system tools: `netstat`, `lsof`, `ping`, `host`, `route`, `curl`
- Python 3 + `fpdf2` (PDF generation only)

## License

MIT
