gap> START_TEST("bugfix.tst");

# verify GeneratorsOfGroup work (reported by Zachary Stier)
gap> U := UnipotChevSubGr("B", 3, GF(3));
<Unipotent subgroup of a Chevalley group of type B3 over GF(3)>
gap> GeneratorsOfGroup(U);
[ x_{1}( Z(3)^0 ), x_{1}( Z(3) ), x_{2}( Z(3)^0 ), x_{2}( Z(3) ), 
  x_{3}( Z(3)^0 ), x_{3}( Z(3) ), x_{4}( Z(3)^0 ), x_{4}( Z(3) ), 
  x_{5}( Z(3)^0 ), x_{5}( Z(3) ), x_{6}( Z(3)^0 ), x_{6}( Z(3) ), 
  x_{7}( Z(3)^0 ), x_{7}( Z(3) ), x_{8}( Z(3)^0 ), x_{8}( Z(3) ), 
  x_{9}( Z(3)^0 ), x_{9}( Z(3) ) ]

#
gap> STOP_TEST("bugfix.tst", 0);