event e;
variable q : nat (40);
property
q = 0;
[](<{true}>last -> q = 39);
rule
on e when q=0 ensure q=1;
on e when q=1 ensure q=2;
on e when q=2 ensure q=3;
on e when q=3 ensure q=4;
on e when q=4 ensure q=5;
on e when q=5 ensure q=6;
on e when q=6 ensure q=7;
on e when q=7 ensure q=8;
on e when q=8 ensure q=9;
on e when q=9 ensure q=10;
on e when q=10 ensure q=11;
on e when q=11 ensure q=12;
on e when q=12 ensure q=13;
on e when q=13 ensure q=14;
on e when q=14 ensure q=15;
on e when q=15 ensure q=16;
on e when q=16 ensure q=17;
on e when q=17 ensure q=18;
on e when q=18 ensure q=19;
on e when q=19 ensure q=20;
on e when q=20 ensure q=21;
on e when q=21 ensure q=22;
on e when q=22 ensure q=23;
on e when q=23 ensure q=24;
on e when q=24 ensure q=25;
on e when q=25 ensure q=26;
on e when q=26 ensure q=27;
on e when q=27 ensure q=28;
on e when q=28 ensure q=29;
on e when q=29 ensure q=30;
on e when q=30 ensure q=31;
on e when q=31 ensure q=32;
on e when q=32 ensure q=33;
on e when q=33 ensure q=34;
on e when q=34 ensure q=35;
on e when q=35 ensure q=36;
on e when q=36 ensure q=37;
on e when q=37 ensure q=38;
on e when q=38 ensure q=39;
on e when q=39 ensure false;
