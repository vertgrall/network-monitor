#!/usr/bin/env python3
"""Generate NT Sentinel (network-monitor) command reference PDF."""

from pathlib import Path

from fpdf import FPDF


class Doc(FPDF):
    def header(self):
        self.set_font("Helvetica", "B", 11)
        self.set_text_color(80, 80, 80)
        self.cell(0, 8, "NT Sentinel - Command Reference", align="R")
        self.ln(12)

    def footer(self):
        self.set_y(-15)
        self.set_font("Helvetica", "I", 9)
        self.set_text_color(120, 120, 120)
        self.cell(0, 10, f"Page {self.page_no()}", align="C")

    def section(self, title: str) -> None:
        self.ln(3)
        self.set_font("Helvetica", "B", 14)
        self.set_text_color(20, 20, 20)
        self.cell(0, 8, title)
        self.ln(8)

    def body(self, text: str) -> None:
        self.set_font("Helvetica", "", 11)
        self.set_text_color(30, 30, 30)
        self.multi_cell(0, 6, text)
        self.ln(2)

    def code_block(self, text: str) -> None:
        self.set_font("Courier", "", 9)
        self.set_fill_color(245, 245, 245)
        self.set_text_color(10, 10, 10)
        for line in text.strip().splitlines():
            self.cell(0, 5.5, "  " + line, ln=True, fill=True)
        self.ln(3)

    def table_row(self, cols: list[str], bold: bool = False, widths: list[int] | None = None) -> None:
        style = "B" if bold else ""
        self.set_font("Helvetica", style, 9)
        if widths is None:
            widths = [55, 125]
        for i, col in enumerate(cols):
            self.cell(widths[i], 6.5, col, border=1)
        self.ln()


def build_pdf(output: Path) -> None:
    pdf = Doc()
    pdf.set_auto_page_break(auto=True, margin=18)
    pdf.add_page()

    pdf.set_font("Helvetica", "B", 22)
    pdf.set_text_color(0, 0, 0)
    pdf.cell(0, 12, "NT Sentinel")
    pdf.ln(10)
    pdf.set_font("Helvetica", "", 12)
    pdf.set_text_color(60, 60, 60)
    pdf.multi_cell(
        0,
        7,
        "network-monitor - CLI network monitoring for macOS and Linux.\n"
        "Project: ~/network-monitor\n"
        "Config: ~/.config/new-tower/session.conf",
    )
    pdf.ln(4)

    pdf.section("1. Build and run")
    pdf.body("Build once, then run commands through cabal:")
    pdf.code_block(
        """cd ~/network-monitor
cabal build
cabal run network-monitor -- menu
cabal run network-monitor -- --help"""
    )
    pdf.body("Optional global install:")
    pdf.code_block(
        """cabal install network-monitor --installdir=~/.local/bin
network-monitor flow"""
    )

    pdf.section("2. Interactive menu")
    pdf.body("Launch the full menu with ASCII header and saved session defaults:")
    pdf.code_block("cabal run network-monitor -- menu")
    pdf.table_row(["#", "Feature"], bold=True, widths=[12, 168])
    menu_rows = [
        ("1", "Interface Statistics"),
        ("2", "Live Bandwidth Monitor (Watch)"),
        ("3", "TCP Connections"),
        ("4", "Visual NetView"),
        ("5", "Traffic Flow (Emit Monitor)"),
        ("6", "Network Dashboard"),
        ("7", "Ping Host"),
        ("8", "Port Check"),
        ("9", "Top Remote Hosts"),
        ("10", "Configure Session Options"),
        ("11", "Help"),
        ("12", "Mission Control"),
        ("13", "App Traffic Summary"),
        ("14", "Traceroute"),
        ("15", "DNS Trace"),
        ("16", "Listening Ports"),
        ("17", "Network Intel"),
        ("18", "Multi-Ping Board"),
        ("19", "Export Session Report"),
        ("0", "Exit"),
    ]
    for num, label in menu_rows:
        pdf.table_row([num, label], widths=[12, 168])
    pdf.ln(2)

    pdf.section("3. Commands overview")
    pdf.table_row(["Command", "Description"], bold=True)
    commands = [
        ("interfaces (if)", "Interface byte and packet counters"),
        ("watch (w)", "Live bandwidth monitor with spark bars"),
        ("connections (conn)", "Active TCP connections"),
        ("dashboard (dash)", "One-screen network status overview"),
        ("netview (nv)", "Live visual network topology map"),
        ("flow (emit)", "Live outbound traffic / emit monitor"),
        ("trace", "Traceroute to a host"),
        ("dns", "DNS record trace for a domain"),
        ("listen", "Local listening ports and owning processes"),
        ("apps", "Live per-app traffic summary"),
        ("mission", "All-in-one mission control panel"),
        ("intel", "Public IP, gateway, Wi-Fi, VPN snapshot"),
        ("report", "Export session report to ~/.config/new-tower/reports/"),
        ("mping", "Multi-target ping board"),
        ("ping", "Ping latency statistics"),
        ("top", "Top remote hosts by connection count"),
        ("port", "Check if a TCP port is open"),
        ("menu", "Interactive menu"),
    ]
    for cmd, desc in commands:
        pdf.table_row([cmd, desc])
    pdf.ln(2)

    pdf.section("4. Traffic flow (emit monitor)")
    pdf.body(
        "Shows outbound bytes/sec per remote host, active flows, process names, "
        "service labels, alerts, DNS names, and per-app emitters."
    )
    pdf.code_block(
        """cabal run network-monitor -- flow
cabal run network-monitor -- flow -I en0 -i 2
cabal run network-monitor -- flow --block bad.example --emit-alert 524288 --log"""
    )

    pdf.section("5. Mission control and intel")
    pdf.code_block(
        """cabal run network-monitor -- mission
cabal run network-monitor -- intel
cabal run network-monitor -- apps
cabal run network-monitor -- report"""
    )

    pdf.section("6. Diagnostics")
    pdf.code_block(
        """cabal run network-monitor -- trace -H 8.8.8.8
cabal run network-monitor -- dns -H google.com
cabal run network-monitor -- listen
cabal run network-monitor -- ping -H 1.1.1.1 --pings 4
cabal run network-monitor -- port -H google.com -p 443
cabal run network-monitor -- top"""
    )

    pdf.section("7. Multi-ping board")
    pdf.code_block(
        """cabal run network-monitor -- mping -F 8.8.8.8 -F 1.1.1.1
# Favorites also load from session.conf (menu option 10, field 6)"""
    )

    pdf.add_page()

    pdf.section("8. Watch mode (continuous)")
    pdf.code_block(
        """cabal run network-monitor -- watch -I en0
cabal run network-monitor -- watch -I en0 -i 2
cabal run network-monitor -- watch -I en0 -c 10
cabal run network-monitor -- watch"""
    )
    pdf.body("DOWN = incoming bytes/sec.  UP = outgoing bytes/sec.")

    pdf.section("9. Interface stats")
    pdf.code_block(
        """cabal run network-monitor -- interfaces
cabal run network-monitor -- interfaces -I en0"""
    )

    pdf.section("10. TCP connections")
    pdf.code_block(
        """cabal run network-monitor -- connections
cabal run network-monitor -- connections -l 10
cabal run network-monitor -- connections -s LISTEN"""
    )

    pdf.section("11. Session configuration")
    pdf.body("Saved at ~/.config/new-tower/session.conf. Edit via menu option 10.")
    pdf.table_row(["Key", "Default / purpose"], bold=True, widths=[45, 135])
    session_rows = [
        ("interface", "en0 - default interface filter"),
        ("interval", "1.0 - refresh seconds"),
        ("count", "0 - auto-stop after N refreshes"),
        ("state", "ESTABLISHED - connection filter"),
        ("limit", "50 - max connection rows"),
        ("favorites", "8.8.8.8,1.1.1.1 - multi-ping targets"),
        ("blocklist", "Alert when flows match host prefix"),
        ("emit_alert", "524288 - high emit rate alert (B/s)"),
        ("logging", "true - append to sessions.log"),
    ]
    for key, desc in session_rows:
        pdf.table_row([key, desc], widths=[45, 135])
    pdf.ln(2)
    pdf.body("Logs and reports:")
    pdf.code_block(
        """~/.config/new-tower/sessions.log
~/.config/new-tower/reports/"""
    )

    pdf.section("12. Command-line options")
    pdf.table_row(["Flag", "Default / meaning"], bold=True)
    options = [
        ("-i, --interval SECS", "Refresh interval (default: 1.0)"),
        ("-c, --count N", "Refresh count; 0 = until Ctrl-C"),
        ("-I, --interface IFACE", "Limit to interface(s), e.g. en0"),
        ("-s, --state STATE", "Connection state (default: ESTABLISHED)"),
        ("-l, --limit N", "Max rows (default: 50)"),
        ("-H, --host HOST", "Target host (default: 8.8.8.8)"),
        ("-p, --port PORT", "Target port (default: 443)"),
        ("--pings N", "Ping packet count (default: 4)"),
        ("-F, --favorite HOST", "Favorite host for mping (repeatable)"),
        ("--block HOST", "Blocklisted host prefix (repeatable)"),
        ("--emit-alert BPS", "Emit alert threshold (default: 524288)"),
        ("--log", "Append session events to sessions.log"),
        ("-h, --help", "Show help text"),
    ]
    for flag, meaning in options:
        pdf.table_row([flag, meaning])
    pdf.ln(2)

    pdf.section("13. Interface names (macOS)")
    pdf.code_block("ifconfig -l")
    pdf.table_row(["Name", "Typical use"], bold=True)
    pdf.table_row(["en0", "Wi-Fi"])
    pdf.table_row(["lo0", "Loopback"])
    pdf.ln(2)

    pdf.section("14. Quick reference")
    pdf.table_row(["Goal", "Command"], bold=True)
    quick = [
        ("Interactive menu", "cabal run network-monitor -- menu"),
        ("Live bandwidth", "cabal run network-monitor -- watch -I en0"),
        ("Outbound traffic", "cabal run network-monitor -- flow"),
        ("Mission control", "cabal run network-monitor -- mission"),
        ("Network intel", "cabal run network-monitor -- intel"),
        ("Export report", "cabal run network-monitor -- report"),
        ("Help", "cabal run network-monitor -- --help"),
    ]
    for goal, cmd in quick:
        pdf.table_row([goal, cmd])

    output.parent.mkdir(parents=True, exist_ok=True)
    pdf.output(str(output))


def main() -> None:
    project_pdf = Path.home() / "network-monitor" / "docs" / "network-monitor-commands.pdf"
    desktop_pdf = Path.home() / "Desktop" / "network-monitor-commands.pdf"

    build_pdf(project_pdf)
    build_pdf(desktop_pdf)

    print(project_pdf)
    print(desktop_pdf)


if __name__ == "__main__":
    main()
