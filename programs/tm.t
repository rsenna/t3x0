! Turing Machine
! Nils M Holm, 2022
! Public Domain / 0BSD license

use t3x: t;
use string: str;
use io;

const	LENGTH = 10000;

var	M;
var	Tape::LENGTH;
var	Pos;
var	State;
var	Left, Right;
var	Count;

struct QS = Q0_PRINT, Q0_MOVE, Q0_STATE,
	    Q1_PRINT, Q1_MOVE, Q1_STATE;

! Machine description:
! Each row is one state:
!   ---scan zero---- ----scan one----
! [ print move state print move state ]
! A new state of %1 means halt.

! Touring machine
touring() return [
	[  0,  1,  0,  1,  1,  0 ] ]; ! 0

! Adding machine
add() return [
	[  0,  1, %1,  0,  1,  1 ],   ! 0
	[  0,  1,  2,  1,  1,  1 ],   ! 1
	[  1, %1,  3,  1,  1,  2 ],   ! 2
	[  0, %1,  4,  1, %1,  3 ],   ! 3
	[  0,  1,  0,  1, %1,  4 ] ]; ! 4

! Copying machine
copy() return [
	[  0, %1,  5,  0,  1,  1 ],   ! 0
	[  0,  1,  2,  1,  1,  1 ],   ! 1
	[  1, %1,  3,  1,  1,  2 ],   ! 2
	[  0, %1,  4,  1, %1,  3 ],   ! 3
	[  1,  1,  0,  1, %1,  4 ],   ! 4
	[  0,  1, %1,  1, %1,  5 ] ]; ! 5

! Multiplying machine
multiply() return [
	[  0,  1,  9,  0,  1,  1 ],   ! 0
	[  0,  1,  2,  1,  1,  1 ],   ! 1
	[  0, %1,  7,  0,  1,  3 ],   ! 2
	[  0,  1,  4,  1,  1,  3 ],   ! 3
	[  1, %1,  5,  1,  1,  4 ],   ! 4
	[  0, %1,  6,  1, %1,  5 ],   ! 5
	[  1,  1,  2,  1, %1,  6 ],   ! 6
	[  0, %1,  8,  1, %1,  7 ],   ! 7
	[  0,  1,  0,  1, %1,  8 ],   ! 8
	[  0,  1, %1,  0,  1,  9 ] ]; ! 9

! 4-State Busy Beaver
bb4() return [
	[  1,  1,  1,  1, %1,  1 ],   ! 0
	[  1, %1,  0,  0, %1,  2 ],   ! 1
	[  1,  1, %1,  1, %1,  3 ],   ! 2
	[  1,  1,  3,  0,  1,  0 ] ]; ! 3

inittape(s) do var k;
	t.memfill(Tape, 0, LENGTH);
	Pos := LENGTH ./ 2;
	k := 0;
	while (s::k) do
		Tape::(Pos+k) := s::k = '0'-> 0: 1;
		k := k+1;
	end
	Left := Pos-1;
	Right := Pos + k + 1;
end

showtape() do var i;
	for (i=Left, Right) do
		if (i = Pos) io.writes("_");
		io.writes(Tape::i-> "1": "0");
	end
	io.writes(" Q");
	io.writeln(str.ntoa(State, -10));
end

step() do var q;
	q := M[State];
	ie (Tape::Pos = 0) do
		Tape::Pos := q[Q0_PRINT];
		Pos := Pos + q[Q0_MOVE];
		State := q[Q0_STATE];
	end
	else do
		Tape::Pos := q[Q1_PRINT];
		Pos := Pos + q[Q1_MOVE];
		State := q[Q1_STATE];
	end
	if (Pos .>= Right-1) Right := Right+1;
	if (Pos .<= Left) Left := Left-1;
	if (Pos .>= LENGTH \/ Pos < 0) do
		io.writeln("Operator: attach more tape!");
		halt 1;
	end
	showtape();
	Count := Count+1;
end

run() do
	Count := 0;
	State := 0;
	showtape();
	while (State \= %1) step();
	io.writes(str.ntoa(Count, 10));
	io.writeln(" state transitions");
end

do
	M := bb4();
	inittape("0");
	run();
end
