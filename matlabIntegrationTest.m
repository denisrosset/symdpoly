% Script that constructs the relaxations of the I3322 inequality
% for levels 2 to 5, and runs the level 2 relaxation with
% SeDuMi, SCS and SDPT3
!sbt examples/runMain net.alasc.symdpoly.examples.quantum.I3322App
clear all
load i3322_2_scs.mat
[~, ~, ~, scsInfo] = scs(data, cones);
scsObj = -scsInfo.pobj;
load i3322_2_sdpt3.mat
sdpt3obj = sdpt3(blk, At, C, b);
sdpt3obj = sdpt3obj(1);
load i3322_2_sedumi.mat
[x, y, sedumiInfo] = sedumi(A, b, c, K);
sedumiobj = dot(x, c);
realObj = 1.25094;
tol = 1e-5;
assert(abs(scsObj - realObj) < tol);
assert(abs(sdpt3obj - realObj) < tol);
assert(abs(sedumiobj - realObj) < tol);
