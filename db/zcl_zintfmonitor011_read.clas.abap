class ZCL_ZINTFMONITOR011_READ definition
  public
  create private .

public section.

  class-methods DELETE_DETAILS
    importing
      !IS_DETAILS type ZINTFMONITOR011
    raising
      zcx_intfmonitor .
  class-methods DELETE_LIST
    importing
      !IT_LIST type ZTT_ZINTFMONITOR011
    raising
      zcx_intfmonitor .
  class-methods GET_DETAILS
    importing
      !ID_INTFID type ZINTFMONITOR011-INTFID
      !ID_SPRAS type ZINTFMONITOR011-SPRAS
    returning
      value(RS_RESULT) type ZINTFMONITOR011
    raising
      zcx_intfmonitor .
  class-methods GET_LIST
    importing
      !ID_INTFID type ZINTFMONITOR011-INTFID optional
      !ID_SPRAS type ZINTFMONITOR011-SPRAS optional
    exporting
      !ET_LIST type ZTT_ZINTFMONITOR011
    raising
      zcx_intfmonitor .
  class-methods INIT_BUFFER .
  class-methods SAVE_DETAILS
    importing
      !IS_DETAILS type ZINTFMONITOR011
    raising
      zcx_intfmonitor .
  class-methods SAVE_LIST
    importing
      !IT_LIST type ZTT_ZINTFMONITOR011
    raising
      zcx_intfmonitor .
protected section.
private section.

  types:
     BEGIN OF mtyp_ranges,
 INTFID TYPE RANGE OF ZINTFMONITOR011-INTFID ,
 SPRAS TYPE RANGE OF ZINTFMONITOR011-SPRAS ,
        END   OF mtyp_ranges .

  class-data MS_RANGES type MTYP_RANGES .
  class-data MT_BUFFER type ZTT_ZINTFMONITOR011 .

  class-methods _ADD_RANGE
    importing
      !ID_LOW type ANY
    changing
      !CT_RANGE type ANY TABLE .
ENDCLASS.



CLASS ZCL_ZINTFMONITOR011_READ IMPLEMENTATION.


  method DELETE_DETAILS.
  DATA lt_list      TYPE ZTT_ZINTFMONITOR011 .
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
  endmethod.


  method DELETE_LIST.

  CHECK it_list[] IS NOT INITIAL.
  DELETE ZINTFMONITOR011  FROM TABLE it_list.

  IF NOT sy-subrc IS INITIAL.
    RAISE EXCEPTION TYPE zcx_intfmonitor.
  ENDIF.
  endmethod.


  method GET_DETAILS.

  DATA lt_list      TYPE ZTT_ZINTFMONITOR011 .
  DATA ls_list      LIKE LINE OF lt_list.
  DATA lo_exception TYPE REF TO zcx_intfmonitor.

  READ TABLE mt_buffer INTO rs_result
    WITH KEY INTFID = ID_INTFID
						 SPRAS = ID_SPRAS.

  IF NOT sy-subrc IS INITIAL.

    TRY.
        get_list( EXPORTING  ID_INTFID = ID_INTFID
														 ID_SPRAS = ID_SPRAS
                  IMPORTING  et_list = lt_list ).

      READ TABLE lt_list INDEX 1 INTO ls_list.
      MOVE-CORRESPONDING ls_list TO rs_result.

      CATCH zcx_intfmonitor INTO lo_exception.
        RAISE EXCEPTION lo_exception.
    ENDTRY.

  ENDIF.
  endmethod.


  method GET_LIST.

  CLEAR ms_ranges.

  IF ID_INTFID IS SUPPLIED.
    _add_range( EXPORTING id_low   = ID_INTFID
                CHANGING  ct_range = ms_ranges-INTFID ).
  ENDIF.

  IF ID_SPRAS IS SUPPLIED.
    _add_range( EXPORTING id_low   = ID_SPRAS
                CHANGING  ct_range = ms_ranges-SPRAS ).
  ENDIF.

SELECT * INTO TABLE et_list
  FROM ZINTFMONITOR011
  WHERE INTFID IN ms_ranges-INTFID
    AND SPRAS IN ms_ranges-SPRAS.

  INSERT LINES OF et_list INTO TABLE mt_buffer.
  SORT mt_buffer.
  DELETE ADJACENT DUPLICATES FROM mt_buffer.

  IF et_list IS INITIAL.
    RAISE EXCEPTION TYPE zcx_intfmonitor.
  ENDIF.
  endmethod.


  method INIT_BUFFER.
  CLEAR mt_buffer[].
  endmethod.


  method SAVE_DETAILS.
  DATA lt_list type ZTT_ZINTFMONITOR011 .
  DATA ls_list like line of lt_list.
  DATA lo_exception TYPE REF TO zcx_intfmonitor.

  CHECK is_details IS NOT INITIAL.
  TRY .
      MOVE-CORRESPONDING is_details TO ls_list.
      INSERT ls_list INTO TABLE lt_list[].
      save_list( lt_list[] ).
    CATCH zcx_intfmonitor INTO lo_exception.
      RAISE EXCEPTION lo_exception.
  ENDTRY.
  endmethod.


  method SAVE_LIST.

  CHECK it_list[] IS NOT INITIAL.
  MODIFY ZINTFMONITOR011  FROM TABLE it_list.

  IF NOT sy-subrc IS INITIAL.
    RAISE EXCEPTION TYPE zcx_intfmonitor.
  ENDIF.
  endmethod.


  method _ADD_RANGE.

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
  endmethod.
ENDCLASS.
