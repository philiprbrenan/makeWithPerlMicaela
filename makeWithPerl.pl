#!/usr/bin/perl
#-------------------------------------------------------------------------------
# Make with Perl
# Philip R Brenan at gmail dot com, Appa Apps Ltd, 2017
#-------------------------------------------------------------------------------

=pod

Integrated development environment for Geany or similar editor for compiling
testing, running Perl or Java programs.

A java file ../../aaa/Test.java should be in package com.appaapps.aaa for
package+import to work in references to Test.java.

=cut

use warnings FATAL => qw(all);
use strict;
use Data::Dump qw(dump);
use Getopt::Long;
use File::Basename;

my $compile;                                                                    # Compile
my $run;                                                                        # Run
my $perl;                                                                       # Perl project
my $java;                                                                       # Java project

GetOptions
 ('compile'=>\$compile,
  'run'    =>\$run,
  'perl'   =>\$perl,
  'java'   =>\$java,
 );

#say STDERR "AAAA ", dump({compile=>$compile,run=>$run,perl=>$perl,java=>$java,vector=>$vector});

unless($compile or $run)                                                        # Check action
 {say STDERR "specify --compile or --run";
  exit;
 }

my $file = shift @ARGV;                                                         # File to process

&Preprocess::file($file);                                                       # Vector pre-processing if required

if ($perl)                                                                      # Perl
 {if ($compile)
   {say STDERR "Compile perl $file";
    print STDERR qx(perl -cw "$file");                                          # Syntax check perl
   }
  else
   {print STDERR qx(perl -w  "$file");                                          # Run perl
   }
 }
elsif ($java)                                                                   # Java
 {my ($name, undef, $ext) = fileparse($file, qw(.java));                        # Parse file name
  my $path = qx(pwd) =~ s/\s+\Z//gsr;                                           # Present working directory minus trailing new line
  my @path = split /\//, $path;                                                 # Path components
  my $package = pop @path;                                                      # Package is last component of path
  my $area = join '/', @path;                                                   # Directory to contain compiled classes
  if ($compile)                                                                 # Compile
   {say STDERR "Compile java $file";
    my $c = "javac -d $area  -cp $area -Xlint  -Xdiags:verbose $file";          # Syntax check Java
    print STDERR qx($c);
   }
  else                                                                          # Compile and run
   {my $class = "com.appaapps.$package.$name";                                  # Class location
    my $c = "javac -d $area -cp $area $file && java -cp $area $class";          # Run java
    say STDERR $c;
    print STDERR qx($c);
   }
 }

=pod

If we have a java file in which any line starts with either of:

 //vector
 /*vector

then expand the vector statements and replace the original file so that the
vector operation are all inline and local so that we avoid the method overhead,
the allocation overhead and the garbage collection overhead that Java would
otherwise foist upon us.

=cut

sub Preprocess                                                                  # Vector commands
 {package Preprocess;
  use Carp;
  use Data::Table::Text qw(:all);
  use Data::Dump qw(dump);
  use utf8;

  sub tab  {10}                                                                 # Comment tab width
  sub col  {80}                                                                 # Comment column

  sub file($)                                                                   # Preprocess a file
   {my ($file) = @_;
    return unless $java;                                                        # Vectorization only applied to java files
    my @gen;
    my $source = readFile($file);                                               # Read source file
       $source =~ s/\s+\Z//gs;

    for(split /\n/, readFile($file))                                            # Check each source line
     {my @p = m(\A/[*/](\w+));                                                  # Package name
      if (@p)                                                                   # Preprocess request label either /* or // followed by package
       {my @parms = split /\s+/, $_;                                            # Parse vector command
        my ($label, $command) = @parms;                                         # Label, command
        my $p = bless [@parms], $p[0];                                          # Use package name to bless request
        push @gen, $p->$command;                                                # Save generated code
       }
      else                                                                      # Normal code
       {push @gen, $_;
       }
     }
    if (1)
     {my $s = join("\n", @gen);
         $s =~ s/\s+\Z//gs;

      writeFile($file, $s) unless $s eq $source;                                # Update the source file if we have created a newer version
     }
   }

  sub parmCount($$)                                                             # Complain if there are too few parameters
   {my ($parms, $count) = @_;                                                   # Vector command parameters, relevant parameter count

    confess "Wrong parameter count, expected at least ".($count+2).             # Two few parameters
     " got ".scalar(@$parms).
     " see:\n". join(' ', @$parms)."\n" unless @$parms >= $count+2;

    confess "Undefined parameter ", dump($parms) if grep {!defined} @$parms;    # Undefined parameters
   }

  sub lay($$$$)                                                                 # Layout a line of code so things line up
   {my ($parms, $count, $code, $comment) = @_;                                  # Vector command parameters, relevant parameters count, code, comment
    parmCount($parms, $count);
    my $c = col;
    my $t = tab;
    $#$parms = $count + 1;                                                      # Relevant parameters
    my $name = \$parms->[0];                                                    # Address label
    $$name = '/*'.$$name =~ s/\A[\/*]+//r;                                      # Format label
    $parms->[1] = pad($parms->[1], $t);                                         # Format command name
    my $p = pad(join(' ', @$parms), $t);                                        # Format parameters
    my $s = join ' ', $p, '*/ '.$code;
    my $l = length($s);
    my $n = $l < $c ? $c - $l : $t - $l % $t;                                   # Space text length
    my $T = ' ' x $n;
    $s.$T.'/* '.$comment.' */';                                                 # Layout line of code
   }

  sub subParms($$$)                                                             # Substitute arguments for operands in template and description
   {my ($parms, $count, $string) = @_;                                          # Vector command parameters, relevant parameter count, command, parameters
    parmCount($parms, $count);
    my ($label, $command, @parms) = @$parms;                                    # Arguments to vector operation
    my @chars = split //, "𝗮𝗯𝗰𝗱𝗲𝗳𝗴𝗵𝗶𝗷𝗸𝗹𝗺𝗻𝗼𝗽𝗾𝗿𝘀𝘁𝘂𝘃𝘄𝘅𝘆𝘇";                         # Parameter names
    $#chars   = $count;                                                         # Number of parameters
    for(keys @chars)                                                            # Substitute each parameter
     {my $p = $parms[$_];
      my $c = $chars[$_];
      $string =~ s/$c/$p/g if defined($p);
     }
    $string
   }

  sub gen($$$$)                                                                 # Generate code for a generic vector operation
   {my ($parms, $count, $description, $template) = @_;                          # Vector command parameters, relevant parameter count, description of command, code template
    parmCount($parms, $count);
    $#$parms = $count+1;                                                        # Relevant parameters
    my ($label, $command, $a, $b, $c, $d) = @$parms;                            # Arguments to vector operation
    $$_ = subParms($parms, $count, $$_) for \$template, \$description;          # Substitute arguments for operands in template and description
    if ($template =~ /\.[𝘅𝘆]/)                                                  # x to x and x to y
     {my $x = $template =~ s/\.𝘅/_x/gr;
      my $y = $template =~ s/\.𝘅/_y/gr;
      lay($parms, $count, "$x $y", $description);
     }
    else
     {lay($parms, $count,  $template, $description);
     }
   }

  sub wrap($$$@)                                                                # Generate code for a generic vector operation
   {my ($parms, $count, $description, @statements) = @_;                        # Vector command parameters, relevant parameter count, description of command, code template
    parmCount($parms, $count);
    my $s = nws(join(' ', @statements) =~ s/\/\*.+?\*\///gr);                   # Put statements on one line and remove comments
    lay($parms, $count,  $s, subParms($parms, $count, $description));           # Layout command
   }
 }

my $temporaryIdentifier = 0;
sub temporaryIdentifier {'t_'.(++$temporaryIdentifier)}

sub Vector                                                                      # Vector commands
 {package Vector;
  use Data::Dump qw(dump);
  use Data::Table::Text qw(:all);
  use utf8;

  sub gen {&Preprocess::gen(@_)}                                                # Generate source code for a command
  sub wrap{&Preprocess::wrap(@_)}                                               # Wrap the generated commands so that the source can be regenerated as needed
  sub tmp {::temporaryIdentifier(@_)}                                           # Temporary identifier
  sub vector {my ($command) = @_; bless([__PACKAGE__, @_])->$command}           # Request source code
  sub closeEnough{1e-6}                                                         # Close enough

  sub x      {gen($_[0], 2, 'place x component of vector 𝗮 in scalar 𝗯',                         'final float 𝗯 = 𝗮_x;')}
  sub y      {gen($_[0], 2, 'place y component of vector 𝗮 in scalar 𝗯',                         'final float 𝗯 = 𝗮_y;')}
  sub xy     {gen($_[0], 3, 'place (x, y) components of vector 𝗮 in scalars 𝗯 and 𝗰',            'final float 𝗯 = 𝗮_x, 𝗰 = 𝗮_y;')}
  sub eq     {gen($_[0], 3, 'set scalar 𝗰 to show if vectors 𝗮 and 𝗯 are equal',                  'final boolean 𝗰 = Math.hypot(𝗮_x - 𝗯_x, 𝗮_y - 𝗯_y) < '.closeEnough.';')}
  sub eqFloat{gen($_[0], 3, 'set scalar 𝗰 to show if floats 𝗮 and 𝗯 are equal',                   'final boolean 𝗰 = Math.abs(𝗮 - 𝗯)'.               ' < '.closeEnough.';')}
  sub declare{gen($_[0], 1, 'declare vector 𝗮',                                                  'float 𝗮.𝘅 = 0;')}
  sub assign {gen($_[0], 2, 'assign vector 𝗮 to non final vector 𝗯',                             '𝗯.𝘅 = 𝗮.𝘅;')}
  sub X      {gen($_[0], 2, 'assign to the x component of vector 𝗯 from scalar 𝗮',               '𝗯_x = 𝗮;')}
  sub Y      {gen($_[0], 2, 'assign to the y component of vector 𝗯 from scalar 𝗮',               '𝗯_y = 𝗮;')}
  sub XY     {gen($_[0], 3, 'assign to vector 𝗰 from two scalar operands 𝗮 and 𝗯',               '𝗰_x = 𝗮; 𝗰_y = 𝗯;')}
  sub new    {gen($_[0], 3, 'new vector 𝗰 from two scalar operands 𝗮 and 𝗯',                     'final float 𝗰_x = 𝗮, 𝗰_y = 𝗯;')}
  sub plus   {gen($_[0], 3, 'vector 𝗮 plus '      .'vector 𝗯 giving new vector 𝗰',               'final float 𝗰.𝘅 = 𝗮.𝘅 + 𝗯.𝘅;')}
  sub minus  {gen($_[0], 3, 'vector 𝗮 minus '     .'vector 𝗯 giving new vector 𝗰',               'final float 𝗰.𝘅 = 𝗮.𝘅 - 𝗯.𝘅;')}
  sub times  {gen($_[0], 3, 'vector 𝗮 times '     .'scalar 𝗯 giving new vector 𝗰',               'final float 𝗰.𝘅 = 𝗮.𝘅 * 𝗯;')}
  sub over   {gen($_[0], 3, 'vector 𝗮 divided by '.'scalar 𝗯 giving new vector 𝗰',               'final float 𝗰.𝘅 = 𝗮.𝘅 / 𝗯;')}
  sub mid    {gen($_[0], 3, 'vector 𝗰 is mid way between vector 𝗮 and vector 𝗯',                 'final float 𝗰.𝘅 = (𝗮.𝘅 + 𝗯.𝘅) / 2f;')}

  sub dot    {gen($_[0], 3, 'vector 𝗮 dot vector 𝗯 giving scalar 𝗰',                             'final float 𝗰 = 𝗮_x * 𝗯_x + 𝗮_y * 𝗯_y;')}
  sub det    {gen($_[0], 3, 'vector 𝗮 det vector 𝗯 giving scalar 𝗰',                             'final float 𝗰 = 𝗮_x * 𝗯_y - 𝗮_y * 𝗯_x;')}
  sub detVec {gen($_[0], 5, 'vector 𝗲 = (det of vectors 𝗮 𝗯, det of vectors 𝗰, 𝗱)',              'final float 𝗲_x = 𝗮_x * 𝗯_y - 𝗮_y * 𝗯_x,  𝗲_y = 𝗰_x * 𝗱_y - 𝗰_y * 𝗱_x;')}

  sub length2{gen($_[0], 2, 'squared length of vector 𝗮 giving scalar 𝗯',                        'final float 𝗯 = 𝗮_x * 𝗮_x + 𝗮_y * 𝗮_y;')}
  sub length {gen($_[0], 2,         'length of vector 𝗮 giving scalar 𝗯',                        'final float 𝗯 = (float)Math.hypot(𝗮_x, 𝗮_y);')}

  sub clone  {gen($_[0], 2, 'copy vector 𝗮 to new vector 𝗯',                                     'final float 𝗯_x = 𝗮_x, 𝗯_y = 𝗮_y;')}
  sub rot1   {gen($_[0], 2, 'copy vector 𝗮 to new vector 𝗯 and rotate  90 anti clockwise',       'final float 𝗯_x = -𝗮_y, 𝗯_y =  𝗮_x;')}
  sub rot2   {gen($_[0], 2, 'copy vector 𝗮 to new vector 𝗯 and rotate 180 degrees',              'final float 𝗯_x = -𝗮_x, 𝗯_y = -𝗮_y;')}
  sub rot3   {gen($_[0], 2, 'copy vector 𝗮 to new vector 𝗯 and rotate  90 clockwise',            'final float 𝗯_x =  𝗮_y, 𝗯_y = -𝗮_x;')}
  sub about1 {gen($_[0], 3, 'rotate vector 𝗯 around vector 𝗮  90 anti clockwise giving vector 𝗰', 'final float 𝗰_x = 𝗮_x-(𝗯_y-𝗮_y), 𝗰_y = 𝗮_y+(𝗯_x-𝗮_x);')}
  sub about2 {gen($_[0], 3, 'rotate vector 𝗯 around vector 𝗮 180 degrees '.    'giving vector 𝗰', 'final float 𝗰_x = 𝗮_x-(𝗯_x-𝗮_x), 𝗰_y = 𝗮_y-(𝗯_y-𝗮_y);')}
  sub about3 {gen($_[0], 3, 'rotate vector 𝗯 around vector 𝗮   90 clockwise '.  'giving vector 𝗰', 'final float 𝗰_x = 𝗮_x+(𝗯_y-𝗮_y), 𝗰_y = 𝗮_y-(𝗯_x-𝗮_x);')}

  sub distanceTo
   {my ($parms) = @_;
    my ($label, $command, $a, $b, $c) = @$parms;
    my $t = tmp;
    wrap($parms, 3, "Put the distance from vector 𝗮 to vector 𝗯 in scalar 𝗰",
      vector(qw(minus), $b, $a, $t).
      vector(qw(length), $t, $c));
   }

  sub norm
   {my ($parms) = @_;
    my ($label, $command, $a, $b, $c) = @$parms;
    my ($t1, $t2) = (tmp, tmp);
    wrap($parms, 2, "copy vector 𝗮 to new vector 𝗯 and normalize",
      vector(qw(clone),  $a,  $t1).
      vector(qw(length), $a,  $t2).
      vector(qw(over),   $t1, $t2, $b));
   }

  sub angle                                                                     # sin(angle abc)
   {my ($parms) = @_;
    my ($label, $command, $a, $b, $c, $d) = @$parms;
    my ($ab, $cb, $abn, $cbn, $angle) = (tmp,tmp,tmp,tmp);
    wrap($parms, 4, "scalar 𝗱 = sin(angle between points 𝗮, 𝗯, 𝗰)",
      vector(qw(minus),   $a,  $b,  $ab).
      vector(qw(minus),   $c,  $b,  $cb).
      vector(qw(norm),    $ab, $abn).
      vector(qw(norm),    $cb, $cbn).
      vector(qw(det),    $abn, $cbn, $d));
   }
# https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
  sub intersectionOfTwoLines                                                    # The intersection of lines through points (𝗮, 𝗰) and (𝗯, 𝗱)
   {my ($parms) = @_;
    my ($label, $command, $a, $b, $c, $d, $e, $f) = @$parms;
    my ($ab, $cd, $x, $y, $D, $E, $F, $R) = map {tmp} 1..8;
    wrap($parms, 6, "The intersection of the lines through points (𝗮, 𝗯) and (𝗰, 𝗱) is placed in vector 𝗳, boolean 𝗲 is set if they are parallel or co-incident",
      vector(qw(minus),   $a,  $b,  $ab).             # along line 1
      vector(qw(minus),   $c,  $d,  $cd).             # along line 2
      vector(qw(det),     $ab, $cd, $D).              # super determinant zero if lines are parallel
      vector(qw(eqFloat), $D,  0,   $e).              # set e if parallel
      vector(qw(declare), $f).                        # declare result area
      "if (!$e) {".                                   # If not parallel lines
        vector(qw(new),   $ab."_x", $cd."_x", $x).    # x stack
        vector(qw(new),   $ab."_y", $cd."_y", $y).    # y stack
        vector(qw(detVec), $a, $b, $c, $d, $E).       # det vector
        vector(qw(detVec), $E, $x, $E, $y, $F).       # det vector
        vector(qw(over),   $F, $D, $R).               # intersection
        vector(qw(assign), $R, $f).                   # assign result
      "}");                                           # end if not parallel lines
   }

  sub pointOnLineClosestToPoint
   {my ($parms) = @_;
    my ($label, $command, $a, $b, $c, $d) = @$parms;
    my ($A, $B, $parallel, $R) = map {tmp} 1..4;
    wrap($parms, 4, "The point 𝗱 on the line through points (𝗯, 𝗰) nearest to point 𝗮",
      vector(qw(minus),   $c,  $b,  $A).              # Vector along line
      vector(qw(rot1),    $A,  $R).                   # Right angles to line
      vector(qw(plus),    $a,  $R, $B).               # to point B
      vector(qw(intersectionOfTwoLines), $a, $B, $b, $c, $parallel, $d). # Intersection of line and normal to line through vector a
     '');
   }

  sub distanceToLine
   {my ($parms) = @_;
    my ($label, $command, $a, $b, $c, $d) = @$parms;
    my ($D) = map {tmp} 1..1;
    wrap($parms, 4, "The shortest distance 𝗱 from the point 𝗮 to the line through the points (𝗯, 𝗰)",
      vector(qw(pointOnLineClosestToPoint), $a, $b, $c, $D). # Vector to line
      vector(qw(distanceTo),                $a, $D, $d).     # Distance
     '');
   }
 }
