# Trndi - turns assets/web_dashboard.html into inc/web_dashboard.inc.
#
# Run through `make dashboard` (make.ps1 has a PowerShell twin that must
# produce byte-identical output, see `make check-dashboard`). The HTML file's
# leading <!-- --> block is the Trndi license header: it is emitted as Pascal
# comment lines so the header stays on the generated file without being served.
# Every other line becomes one string literal; the sum is a compile-time
# constant, so the page costs the binary exactly its own bytes.
#
# Copyright (c) Björn Lindh - GPLv3, see the header of any Trndi source file.
BEGIN { q = "'"; hdr = 0; started = 0 }
NR == 1 && /^<!--/ { hdr = 1 }
hdr == 1 {
  s = $0
  sub(/^<!--/, "", s)
  sub(/ *-->$/, "", s)
  print "//" s
  if ($0 ~ /-->/) {
    hdr = 0
    print "//"
    print "// GENERATED FILE - do not edit. Source: assets/web_dashboard.html;"
    print "// regenerate with `make dashboard` (Unix) or `.\\make.ps1 dashboard` (Windows)."
    print "//"
    print "// The embedded web dashboard served by trndi.webserver.threaded at GET /."
    print "WEB_DASHBOARD_HTML ="
    started = 1
  }
  next
}
{
  if (!started) { print "WEB_DASHBOARD_HTML ="; started = 1 }
  s = $0
  # Split long lines before escaping quotes, so an escaped quote never
  # straddles two literals.
  while (length(s) > 200) {
    p = substr(s, 1, 200)
    gsub(q, q q, p)
    print "  " q p q " +"
    s = substr(s, 201)
  }
  gsub(q, q q, s)
  print "  " q s q " + LineEnding +"
}
END { print "  " q q ";" }
