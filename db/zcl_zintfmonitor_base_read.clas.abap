"! <p class="shorttext synchronized" lang="en">Base DB helper engine</p>
CLASS zcl_zintfmonitor_base_read DEFINITION
  PUBLIC

  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
    "! <p class="shorttext synchronized" lang="en">Add to Range</p>
    CLASS-METHODS _add_range
      IMPORTING id_low   TYPE any
      CHANGING  ct_range TYPE ANY TABLE .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_zintfmonitor_base_read IMPLEMENTATION.

  METHOD _add_range.

    FIELD-SYMBOLS: <ls_row>    TYPE any,
                   <ld_sign>   TYPE any,
                   <ld_option> TYPE any,
                   <ld_low>    TYPE any.

    INSERT INITIAL LINE INTO TABLE ct_range ASSIGNING <ls_row>.

    ASSIGN COMPONENT 'LOW' OF STRUCTURE <ls_row> TO <ld_low>.
    <ld_low> = id_low.

    ASSIGN COMPONENT 'SIGN' OF STRUCTURE <ls_row> TO <ld_sign>.
    <ld_sign> = 'I'.

    ASSIGN COMPONENT 'OPTION' OF STRUCTURE <ls_row> TO <ld_option>.
    <ld_option> = 'EQ'.
  ENDMETHOD.
ENDCLASS.
