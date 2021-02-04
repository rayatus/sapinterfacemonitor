*&---------------------------------------------------------------------*
*& Report zintfmonitor_gui_simgh
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zintfmonitor_gui_simgh.


START-OF-SELECTION.

  CALL FUNCTION 'STREE_EXTERNAL_DISPLAY'
    EXPORTING
      structure_id = '080027E74AD01EDA9BE7CF7E0B1D9173'.
