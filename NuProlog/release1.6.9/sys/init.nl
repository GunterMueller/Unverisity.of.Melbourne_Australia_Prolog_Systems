/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% Nepolog initialization.

?- initializing, $sysOp(Op, Prec, Type), op(Prec, Type, Op), fail.

$sysOp(-->, 1200, xfx).
$sysOp(:-, 1200, fx).
$sysOp(:-, 1200, xfx).
$sysOp(?-, 1200, fx).
$sysOp(pred, 1180, fx).
$sysOp(type, 1180, fx).
$sysOp(useIf, 1180, fx).
%$sysOp(::, 1175, fx).
%$sysOp(::, 1175, xfx).
$sysOp(:, 1175, fx).
$sysOp(:, 1175, xfx).
$sysOp(delete, 1175, fy).
$sysOp(insert, 1175, fy).
$sysOp(update, 1175, fy).
$sysOp(where, 1175, xfx).
$sysOp(in, 1172, xfx).
$sysOp(sorted, 1171, xf).
$sysOp(sorted, 1171, xfx).
$sysOp(else, 1170, xfy).
$sysOp(if, 1160, fx).
$sysOp(dynamic, 1150, fy).
$sysOp(then, 1150, xfx).
$sysOp(pure, 1150, fy).
$sysOp(;, 1100, xfy).
$sysOp(->, 1050, xfy).
$sysOp((,), 1000, xfy).
$sysOp(to, 980, xfx).
$sysOp(all, 950, fxy).
$sysOp(gAll, 950, fxy).
$sysOp(gSome, 950, fxy).
$sysOp(some, 950, fxy).
$sysOp(<=, 920, xfy).
$sysOp(<=>, 920, xfy).
$sysOp(=>, 920, xfy).
$sysOp(\+, 900, fy).
$sysOp(lib, 900, fy).
$sysOp(listing, 900, fy).
$sysOp(man, 900, fy).
$sysOp(nospy, 900, fy).
$sysOp(not, 900, fy).
$sysOp(once, 900, fy).
$sysOp(spy, 900, fy).
$sysOp(wait, 900, fy).
$sysOp(when, 900, xfx).
$sysOp(~, 900, fy).
$sysOp(or, 740, xfy).
$sysOp(and, 720, xfy).
$sysOp(<, 700, xfx).
$sysOp(=, 700, xfx).
$sysOp(=.., 700, xfx).
$sysOp(=:=, 700, xfx).
$sysOp(=<, 700, xfx).
$sysOp(==, 700, xfx).
$sysOp(=\=, 700, xfx).
$sysOp(>, 700, xfx).
$sysOp(>=, 700, xfx).
$sysOp(@<, 700, xfx).
$sysOp(@=<, 700, xfx).
$sysOp(@>, 700, xfx).
$sysOp(@>=, 700, xfx).
$sysOp(\=, 700, xfx).
$sysOp(\==, 700, xfx).
$sysOp(is, 700, xfx).
$sysOp(~=, 700, xfx).
$sysOp(., 600, xfy).
%$sysOp(:, 600, xfy).
$sysOp(+, 500, fx).
$sysOp(+, 500, yfx).
$sysOp(-, 500, fx).
$sysOp(-, 500, yfx).
$sysOp(/\, 500, yfx).
$sysOp(\, 500, fx).
$sysOp(\/, 500, yfx).
$sysOp(*, 400, yfx).
$sysOp(/, 400, yfx).
$sysOp(//, 400, yfx).
$sysOp(<<, 400, yfx).
$sysOp(>>, 400, yfx).
$sysOp(**, 300, xfy).
$sysOp(mod, 300, xfx).
$sysOp(^, 200, xfy).
