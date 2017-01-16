﻿* Encoding: UTF-8.
*EXPLORACE KLASTRŮ VOLIČŮ, KTEŘÍ CHTĚJÍ JÍT K VOLBÁM, ALE NEVÍ KOHO BUDOU VOLIT

DATASET ACTIVATE DataSet1.
USE ALL.
COMPUTE filter_$=((PV_1 = 1 | PV_1 = 2) & PV_4 = 99).
VARIABLE LABELS filter_$ '(PV_1 = 1 | PV_1 = 2) & PV_4 = 99 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

CROSSTABS
  /TABLES=PV_4 BY PV_1
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT ROW 
  /COUNT ROUND CELL.

MISSING VALUES PI_1a PI_1b OV_1 EV_10 PS_1 PO_45b OV_132 (0,9).
MISSING VALUES PO_2 (0,99).

fre PI_1a PI_1b OV_1 IDE_1 EV_10 PS_1 PO_45b OV_132 PO_2 IDE_2 IDE_6.


QUICK CLUSTER PI_1a PI_1b OV_1 IDE_1 EV_10 PS_1 PO_45b OV_132 PO_2 IDE_2 IDE_6
  /MISSING=LISTWISE
  /CRITERIA=CLUSTER(2) MXITER(10) CONVERGE(0)
  /METHOD=KMEANS(NOUPDATE)
  /PRINT INITIAL ANOVA.

*Vyřazení věku.
QUICK CLUSTER PI_1a PI_1b OV_1 IDE_1 EV_10 PS_1 PO_45b OV_132 PO_2 IDE_6
  /MISSING=LISTWISE
  /CRITERIA=CLUSTER(2) MXITER(10) CONVERGE(0)
  /METHOD=KMEANS(NOUPDATE)
  /PRINT INITIAL ANOVA.

*Vyřazení důvěry vládě.
QUICK CLUSTER PI_1a OV_1 IDE_1 EV_10 PS_1 PO_45b OV_132 PO_2 IDE_6
  /MISSING=LISTWISE
  /CRITERIA=CLUSTER(2) MXITER(10) CONVERGE(0)
  /METHOD=KMEANS(NOUPDATE)
  /PRINT INITIAL ANOVA.

*Vyřazení zájmu o politické dění.
QUICK CLUSTER PI_1a OV_1 IDE_1 EV_10 PS_1 OV_132 PO_2 IDE_6
  /MISSING=LISTWISE
  /CRITERIA=CLUSTER(2) MXITER(10) CONVERGE(0)
  /METHOD=KMEANS(NOUPDATE)
  /PRINT INITIAL ANOVA.
