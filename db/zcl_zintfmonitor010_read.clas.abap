CLASS zcl_zintfmonitor010_read DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS get_details
      IMPORTING
        !id_intfid       TYPE zintfmonitor010-intfid
      RETURNING
        VALUE(rs_result) TYPE zintfmonitor010
      RAISING
        zcx_intfmonitor .
    CLASS-METHODS get_list
      IMPORTING
        !id_intfid TYPE zintfmonitor010-intfid OPTIONAL
      EXPORTING
        !et_list   TYPE ztt_zintfmonitor010
      RAISING
        zcx_intfmonitor .
    CLASS-METHODS init_buffer .

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF mtyp_ranges,
        intfid TYPE RANGE OF zintfmonitor010-intfid,
      END   OF mtyp_ranges .

    CLASS-DATA ms_ranges TYPE mtyp_ranges .
    CLASS-DATA mt_buffer TYPE ztt_zintfmonitor010 .

    CLASS-METHODS _add_range
      IMPORTING
        !id_low   TYPE any
      CHANGING
        !ct_range TYPE ANY TABLE .
ENDCLASS.



CLASS zcl_zintfmonitor010_read IMPLEMENTATION.

  METHOD get_details.

    DATA lt_list      TYPE ztt_zintfmonitor010 .
    DATA ls_list      LIKE LINE OF lt_list.
    DATA lo_exception TYPE REF TO zcx_intfmonitor.

    READ TABLE mt_buffer INTO rs_result
      WITH KEY intfid = id_intfid.

    IF NOT sy-subrc IS INITIAL.

      TRY.
          get_list( EXPORTING  id_intfid = id_intfid
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

    SELECT * INTO TABLE et_list
      FROM zintfmonitor010
      WHERE intfid IN ms_ranges-intfid.

    INSERT LINES OF et_list INTO TABLE mt_buffer.
    SORT mt_buffer.
    DELETE ADJACENT DUPLICATES FROM mt_buffer.

    IF et_list IS INITIAL.
      RAISE EXCEPTION TYPE cx_os_db.
    ENDIF.
  ENDMETHOD.


  METHOD init_buffer.
    CLEAR mt_buffer[].
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
