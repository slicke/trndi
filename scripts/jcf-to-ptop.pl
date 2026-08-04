#!/usr/bin/env perl
# Derive a ptop (FPC "pascal to pascal" beautifier) config from JCFSettings.xml,
# so the JEDI Code Formatter profile stays the single source of truth for style.
#
#   perl scripts/jcf-to-ptop.pl                 # write ptop.cfg from JCFSettings.xml
#   perl scripts/jcf-to-ptop.pl -o - --args     # print the config / the ptop flags
#
# ptop understands far fewer knobs than JCF: it can re-indent, re-case keywords,
# insert/suppress line breaks and blank lines, and wrap long lines. Everything
# else in JCFSettings.xml (per-context colon spacing, alignment, uses sorting,
# begin/end removal, first-level indent, comment indentation, …) has no ptop
# equivalent and is listed as "not mapped" in the generated header. ptop is a
# rough pre-formatter, not a replacement for JCF.
use strict;
use warnings;
use File::Basename;

my $in    = undef;
my $out   = undef;
my $args_only = 0;
while (@ARGV) {
  my $a = shift @ARGV;
  if    ($a eq '-o')     { $out = shift @ARGV; }
  elsif ($a eq '--args') { $args_only = 1; }
  elsif ($a eq '-h' || $a eq '--help') {
    print "usage: jcf-to-ptop.pl [JCFSettings.xml] [-o ptop.cfg|-] [--args]\n";
    exit 0;
  }
  else { $in = $a; }
}

my $root = dirname(dirname(__FILE__));
$in  = "$root/JCFSettings.xml" unless defined $in;
$out = "$root/ptop.cfg"        unless defined $out;

# ---------------------------------------------------------------------------
# Read JCFSettings.xml into $cfg{Section}{Key}. The file is written by JCF as
# one <Key> value </Key> per line inside a one-level-deep section element.
# ---------------------------------------------------------------------------
open my $fh, '<', $in or die "jcf-to-ptop: cannot read $in: $!\n";
my (%cfg, $section);
while (<$fh>) {
  if (m{^\s*<(/?)([A-Za-z]+)\s*>\s*$}) {
    my ($close, $name) = ($1, $2);
    next if $name eq 'JediCodeFormatSettings';
    $section = $close ? undef : $name;
    next;
  }
  if (m{^\s*<([A-Za-z]+)>\s*(.*?)\s*</\1>\s*$}) {
    my ($key, $val) = ($1, $2);
    $cfg{ defined $section ? $section : '_' }{$key} = $val;
  }
}
close $fh;

sub val {
  my ($sec, $key, $default) = @_;
  my $v = $cfg{$sec}{$key};
  return $default unless defined $v && $v ne '';
  return $v;
}
sub flag { my $v = val(@_); return lc($v) eq 'true' ? 1 : 0; }
sub num  { my $v = val(@_); return $v + 0; }

# ---------------------------------------------------------------------------
# What we can carry over
# ---------------------------------------------------------------------------
# TCapitalisationType = (ctUpper, ctLower, ctMixed, ctLeaveAlone)
my @CAPS_ATTR = ('upper', 'lower', 'capital', '');
my $caps_on   = flag('Capitalisation', 'Enabled', 'True');
my $caps_word = $caps_on ? $CAPS_ATTR[ num('Capitalisation', 'ReservedWords', 3) ] : '';
my $caps_op   = $caps_on ? $CAPS_ATTR[ num('Capitalisation', 'Operators',     3) ] : '';
my $caps_dir  = $caps_on ? $CAPS_ATTR[ num('Capitalisation', 'Directives',    3) ] : '';

# TWhenToRebreakLines = (rbOff, rbOnlyIfGood, rbUsually)
my $rebreak      = num('Returns', 'WhenRebreakLines', 0);
my $max_line     = num('Returns', 'MaxLineLength', 0);
my $add_returns  = flag('Returns', 'AddGoodReturns', 'False');
my $rm_bad_ret   = flag('Returns', 'RemoveBadReturns', 'False');
my $blank_before = num('Returns', 'LinesBeforeProcedure', 0) > 0 ? 1 : 0;
my $fix_spacing  = flag('Spaces', 'FixSpacing', 'False');
my $indent_be    = flag('Indent', 'IndentBeginEnd', 'False');
my $indent       = num('Indent', 'IndentationSpaces', 2);

# ptop wraps at ~100 columns unless told otherwise; JCF with rbOff must not wrap.
my $linesize = ($rebreak > 0 && $max_line > 0) ? $max_line : 1024;

if ($args_only) {
  print "-i $indent -l $linesize\n";
  exit 0;
}

# Tokens ptop knows, split by what JCF would capitalise them as. Symbols are
# left out of casing entirely (ptop's default config marks them "capital",
# which is meaningless for ';' or '(' ).
# 'read' and 'write' are deliberately left out: ptop cases them wherever they
# appear, so a TStream.Write call would be rewritten to .write. JCF only
# touches them where they really are property directives.
my %CASE_CLASS = map { $_ => 'op' } qw(and div in mod not or);
$CASE_CLASS{$_} = 'dir' for qw(inline virtual);
$CASE_CLASS{$_} = 'word' for qw(
  end begin if then else proc var of while do case with for repeat until func
  label const type record string prog asm try finally except raise class object
  constructor destructor inherited property private public protected published
  initialization finalization library interface implementation unit uses arr
  down file goto nil set to casevar ofobject
);

# ---------------------------------------------------------------------------
# ptop's own default table (ptop -g) is the base; JCF settings edit it.
# ---------------------------------------------------------------------------
my (@lines, @notes);
while (my $line = <DATA>) {
  chomp $line;
  next if $line =~ /^\s*$/;
  my ($tok, $rest) = split(/=/, $line, 2);
  $rest = '' unless defined $rest;

  # "[tok]=..." lines are the de-indent key lists, not attribute lists.
  if ($tok =~ /^\[([a-z]+)\]$/) {
    my $owner = $1;
    if ($owner eq 'begin' && !$indent_be) {
      # IndentBeginEnd=False: a begin block sits at the level of the statement
      # that owns it, so begin must de-indent past those keywords first.
      $rest = join(',', split(/,/, $rest), qw(if then else while with for do casevar));
    }
    push @lines, "[$owner]=$rest";
    next;
  }

  my @attr = grep { $_ ne '' } split(/,/, $rest);
  my @keep;
  for my $a (@attr) {
    next if $a =~ /^(?:upper|lower|capital)$/;        # re-added below
    next if $a =~ /^(?:crbefore|crafter)$/ && !$add_returns;
    next if $a eq 'crsupp'     && !$rm_bad_ret;
    next if $a eq 'blinbefore' && !$blank_before;
    next if $a =~ /^(?:spbef|spaft)$/ && !$fix_spacing;
    push @keep, $a;
  }
  my $class = $CASE_CLASS{$tok};
  if (defined $class) {
    my $c = $class eq 'op' ? $caps_op : $class eq 'dir' ? $caps_dir : $caps_word;
    push @keep, $c if $c ne '';
  }
  push @lines, "$tok=" . join(',', @keep);
}

push @notes, "AddGoodReturns=False: dropped every crbefore/crafter (ptop adds no line breaks)"
  unless $add_returns;
push @notes, "RemoveBadReturns=False: dropped every crsupp (ptop removes no line breaks)"
  unless $rm_bad_ret;
push @notes, "FixSpacing=False: dropped every spbef/spaft (ptop keeps the source spacing)"
  unless $fix_spacing;
push @notes, "LinesBeforeProcedure=0: dropped every blinbefore (ptop inserts no blank lines)"
  unless $blank_before;
push @notes, "IndentBeginEnd=False: [begin] also de-indents past if/then/else/while/with/for/do/casevar"
  unless $indent_be;
push @notes, "Capitalisation: reserved words=" . ($caps_word || 'leave alone')
  . ", operators=" . ($caps_op || 'leave alone')
  . ", directives=" . ($caps_dir || 'leave alone');

# Settings with no ptop counterpart at all — worth spelling out, since the
# result is *not* JCF-equivalent output.
my @unmapped = (
  'Indent.FirstLevelIndent (ptop has one uniform indent step)',
  'Indent.IndentCaseLabels / IndentElse / IndentNestedTypes / IndentVarAndConstInClass',
  'Spaces.SpacesBeforeColon* (ptop has no per-context colon spacing)',
  'Spaces.SpaceBeforeOpenBracketsInFunctionDeclaration vs …InFunctionCall',
  'Returns.RemoveConsecutiveBlankLines / RemoveVarBlankLines / RemoveProcHeaderBlankLines',
  'Comments.* (ptop never re-indents multi-line { } comments)',
  'Align.* , Uses.* , Replace.* , SpecificWordCaps.* , Identifiers.*',
  'Transform.BeginEndStyle (removing redundant begin/end is a parse-tree edit)',
  'PreProcessor.DefinedSymbols (ptop does not evaluate $IFDEF branches)',
);

my $fhout;
if ($out eq '-') { $fhout = \*STDOUT; }
else { open $fhout, '>', $out or die "jcf-to-ptop: cannot write $out: $!\n"; }

my $src = basename($in);
print $fhout "# ptop.cfg — GENERATED from $src by scripts/jcf-to-ptop.pl. Do not edit;\n";
print $fhout "# change $src and regenerate. '#' lines are the only comments ptop accepts.\n";
print $fhout "#\n";
print $fhout "# Indent width and line length are command-line only, so always run ptop as:\n";
print $fhout "#     ptop -i $indent -l $linesize -c ptop.cfg <in> <out>\n";
print $fhout "# (without -l, ptop rewraps at ~100 columns; JCF here has rebreaking off.)\n";
print $fhout "# ptop 1.2 also prepends one blank line and drops the final newline, so\n";
print $fhout "# strip/restore those if you diff the result against the input.\n";
print $fhout "#\n";
print $fhout "# Derived from $src:\n";
print $fhout "#   - $_\n" for @notes;
print $fhout "#\n";
print $fhout "# NOT mapped (ptop cannot express these; its output is close to, but not the\n";
print $fhout "# same as, what JCF produces):\n";
print $fhout "#   - $_\n" for @unmapped;
print $fhout "#\n";
print $fhout "$_\n" for @lines;
close $fhout unless $out eq '-';

__DATA__
end=crbefore,dindonkey,dindent,crafter,capital
[end]=if,then,else,while,with,for,record,try,finally,except,class,object,private,public,protected,published,casevar,colon,equals
begin=crbefore,dindonkey,inbytab,crafter,capital
[begin]=var,label,const,type
if=spaft,gobsym,inbytab,capital
then=capital
else=crbefore,dindonkey,inbytab,capital
[else]=if,then,else
proc=dindonkey,spaft,capital
[proc]=var,label,const,type
var=blinbefore,dindonkey,spaft,inbytab,capital
[var]=var,label,const,type
of=crsupp,spbef,spaft,capital
while=spaft,gobsym,inbytab,crafter,capital
do=crsupp,spbef,capital
case=spaft,gobsym,inbytab,crafter,capital
with=spaft,gobsym,inbytab,crafter,capital
for=spaft,gobsym,inbytab,crafter,capital
repeat=inbytab,crafter,capital
until=crbefore,dindonkey,dindent,spaft,gobsym,crafter,capital
[until]=if,then,else,while,with,for,colon,equals
func=dindonkey,spaft,capital
[func]=var,label,const,type
label=blinbefore,spaft,inbytab,capital
const=blinbefore,dindonkey,spaft,inbytab,capital
[const]=var,label,const,type
type=blinbefore,dindonkey,spaft,inbytab,capital
[type]=var,label,const,type
record=inbyindent,crafter,capital
[record]=end
string=
prog=blinbefore,spaft,capital
asm=
try=crbefore,inbytab,crafter,capital
finally=crbefore,dindent,inbytab,crafter,capital
[finally]=try
except=crbefore,dindent,inbytab,crafter,capital
[except]=try
raise=
class=inbyindent,capital
object=inbyindent,capital
constructor=
destructor=
inherited=
property=
private=crbefore,dindonkey,spaft,inbytab,capital
[private]=end,private,public,protected,published
public=crbefore,dindonkey,spaft,inbytab,capital
[public]=end,private,public,protected,published
protected=crbefore,dindonkey,spaft,inbytab,capital
[protected]=end,private,public,protected,published
published=crbefore,dindonkey,spaft,inbytab,capital
[published]=end,private,public,protected,published
initialization=
finalization=
inline=
library=blinbefore,spaft,capital
interface=blinbefore,crafter,capital
implementation=blinbefore,dindonkey,crafter,capital
[implementation]=end,var,label,const,type,property
read=
write=
unit=blinbefore,spaft,capital
and=
arr=
div=
down=
file=
goto=
in=
mod=
not=
nil=
or=
set=
to=
virtual=
uses=blinbefore,spaft,capital
casevar=spaft,gobsym,inbytab,crafter,capital
ofobject=
becomes=spbef,spaft,gobsym,capital
notequal=
lessorequal=
greaterorequal=
delphicomment=crafter
dopencomment=
dclosecomment=
opencomment=crsupp,capital
closecomment=crsupp,capital
semicolon=crsupp,dindonkey,crafter,capital
[semicolon]=if,then,else,while,with,for,colon,equals
colon=inbytab,capital
equals=spbef,spaft,inbytab,capital
openparen=gobsym,capital
closeparen=
period=crsupp,capital
endoffile=
other=
