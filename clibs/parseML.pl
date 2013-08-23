#######################################################################
#                              Project Couverture                     #
#                                                                     #
# file: parseML.pl                                                    #
# authors: Alexis Darrasse                                            #
# licence: CeCIL-B                                                    #
#######################################################################

$list = shift;
$mname = shift;
open LIST, $list;
while (<LIST>) {chomp; $funcs{$_} = -1}
close LIST;
while (<>) {$f .= $_;}
while ($f =~ /external\s*\w*\s*:[^-=]*((->[^-=]*)*)=\s*\"(\w*)\"/gs) {
  $name = $3;
  $args = $1;
  $nargs = 0;
# TODO marche pas quand les arguments sont des fonctions
  $nargs++ while $args =~ /->/gs;
  if ($funcs{$name}) {
    $funcs{$name} = $nargs;
  }
}
print "(* Primitives of $mname *)\n\n";
foreach $name (keys %funcs) {
  if ($funcs{$name} == -1) {
    print "(* Missing primitive $name *)\n";
  } elsif ($funcs{$name} > 5) {
    $funcs{$name} = "n";
    print "external extn_$name : Obj.t array -> int -> Obj.t";
    print " = \"$name\"\n";
    print "external ext_$name : Obj.t -> Obj.t -> Obj.t";
    print " = \"$name\"\n";
  } else {
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
    if ($fst == 1) {
      $fst = 0;
    } else {
      print ";\n";
    }
  }
  if ($funcs{$name} == "n") {
    print "  Ffi.add2 \"$name\" (Ffi.warp2 ext_$name);\n";
    print "  Ffi.addn \"$name\" (Ffi.warpn extn_$name)";
  } elsif ($funcs{$name} != -1) {
    $nargs = $funcs{$name};
    print "  Ffi.add$nargs \"$name\" (Ffi.warp$nargs ext_$name)";
  }
}
print "\n\nlet init dlls_section =\n";
#print "  if List.exists ((=) \"dll$mname\") dlls_section then prims ()\n;;\n";
print "  prims ()\n;;\n";
print "Ffi.init_list := init::!Ffi.init_list\n";
