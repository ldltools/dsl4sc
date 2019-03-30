event e;
variable q : nat (10);
property
q = 0;
[](<{true}>last -> q = 9);
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
on e when q=9 ensure false;
