CLASS zcl_zintfmonitor011_read DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS delete_details
      IMPORTING
        !is_details TYPE zintfmonitor011
      RAISING
        zcx_intfmonitor .
    CLASS-METHODS delete_list
      IMPORTING
        !it_list TYPE ztt_zintfmonitor011
      RAISING
        zcx_intfmonitor .
    CLASS-METHODS get_details
      IMPORTING
        !id_intfid       TYPE zintfmonitor011-intfid
        !id_spras        TYPE zintfmonitor011-spras
      RETURNING
        VALUE(rs_result) TYPE zintfmonitor011
      RAISING
        zcx_intfmonitor .
    CLASS-METHODS get_list
      IMPORTING
        !id_intfid TYPE zintfmonitor011-intfid OPTIONAL
        !id_spras  TYPE zintfmonitor011-spras OPTIONAL
      EXPORTING
        !et_list   TYPE ztt_zintfmonitor011
      RAISING
        zcx_intfmonitor .
    CLASS-METHODS init_buffer .
    CLASS-METHODS save_details
      IMPORTING
        !is_details TYPE zintfmonitor011
      RAISING
        zcx_intfmonitor .
    CLASS-METHODS save_list
      IMPORTING
        !it_list TYPE ztt_zintfmonitor011
      RAISING
        zcx_intfmonitor .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF mtyp_ranges,
        intfid TYPE RANGE OF zintfmonitor011-intfid,
        spras  TYPE RANGE OF zintfmonitor011-spras,
      END   OF mtyp_ranges .

    CLASS-DATA ms_ranges TYPE mtyp_ranges .
    CLASS-DATA mt_buffer TYPE ztt_zintfmonitor011 .

    CLASS-METHODS _add_range
      IMPORTING
        !id_low   TYPE any
      CHANGING
        !ct_range TYPE ANY TABLE .
ENDCLASS.



CLASS zcl_zintfmonitor011_read IMPLEMENTATION.


  METHOD delete_details.
    DATA lt_list      TYPE ztt_zintfmonitor011 .
    DATA ls_list      LIKE LINE OF lt_list.
    DATA lo_exception TYPE REF TO  zcx_intfmonitor.

    CHECK is_details IS NOT INITIAL.
    TRY .
        MOVE-CORRESPONDING is_details TO ls_list.
        INSERT ls_list INTO TABLE lt_list[].
        delete_list( lt_list[] ).
      CATCH zcx_intfmonitor INTO lo_exception.
        RAISE EXCEPTION lo_exception.
    ENDTRY.
  ENDMETHOD.


  METHOD delete_list.

    CHECK it_list[] IS NOT INITIAL.
    DELETE zintfmonitor011  FROM TABLE it_list.

    IF NOT sy-subrc IS INITIAL.
      RAISE EXCEPTION TYPE zcx_intfmonitor.
    ENDIF.
  ENDMETHOD.


  METHOD get_details.

    DATA lt_list      TYPE ztt_zintfmonitor011 .
    DATA ls_list      LIKE LINE OF lt_list.
    DATA lo_exception TYPE REF TO zcx_intfmonitor.

    READ TABLE mt_buffer INTO rs_result
      WITH KEY intfid = id_intfid
  						 spras = id_spras.

    IF NOT sy-subrc IS INITIAL.

      TRY.
          get_list( EXPORTING  id_intfid = id_intfid
  														 id_spras = id_spras
                    IMPORTING  et_list = lt_list ).

          READ TABLE lt_list INDEX 1 INTO ls_list.
          MOVE-CORRESPONDING ls_list TO rs_result.

        CATCH zcx_intfmonitor INTO lo_exception.
          RAISE EXCEPTION lo_exception.
      ENDTRY.

    ENDIF.
  ENDMETHOD.


  METHOD get_list.

    CLEAR ms_ranges.

    IF id_intfid IS SUPPLIED.
      _add_range( EXPORTING id_low   = id_intfid
                  CHANGING  ct_range = ms_ranges-intfid ).
    ENDIF.

    IF id_spras IS SUPPLIED.
      _add_range( EXPORTING id_low   = id_spras
                  CHANGING  ct_range = ms_ranges-spras ).
    ENDIF.

    SELECT * INTO TABLE et_list
      FROM zintfmonitor011
      WHERE intfid IN ms_ranges-intfid
        AND spras IN ms_ranges-spras.

    INSERT LINES OF et_list INTO TABLE mt_buffer.
    SORT mt_buffer.
    DELETE ADJACENT DUPLICATES FROM mt_buffer.

    IF et_list IS INITIAL.
      RAISE EXCEPTION TYPE zcx_intfmonitor.
    ENDIF.
  ENDMETHOD.


  METHOD init_buffer.
    CLEAR mt_buffer[].
  ENDMETHOD.


  METHOD save_details.
    DATA lt_list TYPE ztt_zintfmonitor011 .
    DATA ls_list LIKE LINE OF lt_list.
    DATA lo_exception TYPE REF TO zcx_intfmonitor.

    CHECK is_details IS NOT INITIAL.
    TRY .
        MOVE-CORRESPONDING is_details TO ls_list.
        INSERT ls_list INTO TABLE lt_list[].
        save_list( lt_list[] ).
      CATCH zcx_intfmonitor INTO lo_exception.
        RAISE EXCEPTION lo_exception.
    ENDTRY.
  ENDMETHOD.


  METHOD save_list.

    CHECK it_list[] IS NOT INITIAL.
    MODIFY zintfmonitor011  FROM TABLE it_list.

    IF NOT sy-subrc IS INITIAL.
      RAISE EXCEPTION TYPE zcx_intfmonitor.
    ENDIF.
  ENDMETHOD.


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
