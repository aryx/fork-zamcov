#######################################################################
#                              Project Couverture                     #
#                                                                     #
# file: parseC.pl                                                     #
# authors: Alexis Darrasse                                            #
# licence: CeCIL-B                                                    #
#######################################################################

# TODO n arguments
$fname = shift;
open LIST, "runtime";
while (<LIST>) {chomp; $funcs{$_} = -1}
close LIST;
open SFILE, $fname;
while (<SFILE>) {$f .= $_;}
close SFILE;
# TODO basename $fname
print "(* Primitives from runtime file $fname *)\n\n";
while ($f =~ /value\s+(\w+)\s*\([^,)]*((,[^,)]*)*)\)/gs) {
  $name = $1;
  $args = $2;
  $nargs = 1;
  $nargs++ while $args =~ /,/gs;
  if ($funcs{$name}) {
    $funcs{$name} = $nargs;
  }
}
foreach $name (keys %funcs) {
  if ($funcs{$name} != -1) {
    $nargs = $funcs{$name};
    print "external ext_$name : Obj.t";
    print " -> Obj.t"x$nargs;
    print " = \"$name\"\n";
  }
}

print "\nlet prims () =\n";
$fst = 1;
foreach $name (keys %funcs) {
  if ($funcs{$name} != -1) {
    $nargs = $funcs{$name};
    if ($fst == 1) {
      $fst = 0;
    } else {
      print ";\n";
    }
    print "  Ffi.add$nargs \"$name\" (Ffi.warp$nargs ext_$name)";
  }
}
print "\n\nlet init dlls_section =\n";
print "  prims ()\n;;\n";
print "Ffi.init_list := init::!Ffi.init_list\n";
