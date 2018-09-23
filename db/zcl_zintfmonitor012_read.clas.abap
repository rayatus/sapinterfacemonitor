class ZCL_ZINTFMONITOR012_READ definition
  public
  create private .

public section.

  class-methods DELETE_DETAILS
    importing
      !IS_DETAILS type ZINTFMONITOR012
    raising
      zcx_intfmonitor .
  class-methods DELETE_LIST
    importing
      !IT_LIST type ZTT_ZINTFMONITOR012
    raising
      zcx_intfmonitor .
  class-methods GET_DETAILS
    importing
      !ID_INTFID type ZINTFMONITOR012-INTFID
      !ID_PARAM type ZINTFMONITOR012-PARAM
    returning
      value(RS_RESULT) type ZINTFMONITOR012
    raising
      zcx_intfmonitor .
  class-methods GET_LIST
    importing
      !ID_INTFID type ZINTFMONITOR012-INTFID optional
      !ID_PARAM type ZINTFMONITOR012-PARAM optional
    exporting
      !ET_LIST type ZTT_ZINTFMONITOR012
    raising
      zcx_intfmonitor .
  class-methods INIT_BUFFER .
  class-methods SAVE_DETAILS
    importing
      !IS_DETAILS type ZINTFMONITOR012
    raising
      zcx_intfmonitor .
  class-methods SAVE_LIST
    importing
      !IT_LIST type ZTT_ZINTFMONITOR012
    raising
      zcx_intfmonitor .
protected section.
private section.

  types:
     BEGIN OF mtyp_ranges,
 INTFID TYPE RANGE OF ZINTFMONITOR012-INTFID ,
 PARAM TYPE RANGE OF ZINTFMONITOR012-PARAM ,
        END   OF mtyp_ranges .

  class-data MS_RANGES type MTYP_RANGES .
  class-data MT_BUFFER type ZTT_ZINTFMONITOR012 .

  class-methods _ADD_RANGE
    importing
      !ID_LOW type ANY
    changing
      !CT_RANGE type ANY TABLE .
ENDCLASS.



CLASS ZCL_ZINTFMONITOR012_READ IMPLEMENTATION.


  method DELETE_DETAILS.
  DATA lt_list      TYPE ZTT_ZINTFMONITOR012 .
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
  DELETE ZINTFMONITOR012  FROM TABLE it_list.

  IF NOT sy-subrc IS INITIAL.
    RAISE EXCEPTION TYPE zcx_intfmonitor.
  ENDIF.
  endmethod.


  method GET_DETAILS.

  DATA lt_list      TYPE ZTT_ZINTFMONITOR012 .
  DATA ls_list      LIKE LINE OF lt_list.
  DATA lo_exception TYPE REF TO zcx_intfmonitor.

  READ TABLE mt_buffer INTO rs_result
    WITH KEY INTFID = ID_INTFID
						 PARAM = ID_PARAM.

  IF NOT sy-subrc IS INITIAL.

    TRY.
        get_list( EXPORTING  ID_INTFID = ID_INTFID
														 ID_PARAM = ID_PARAM
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

  IF ID_PARAM IS SUPPLIED.
    _add_range( EXPORTING id_low   = ID_PARAM
                CHANGING  ct_range = ms_ranges-PARAM ).
  ENDIF.

SELECT * INTO TABLE et_list
  FROM ZINTFMONITOR012
  WHERE INTFID IN ms_ranges-INTFID
    AND PARAM IN ms_ranges-PARAM.

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
  DATA lt_list type ZTT_ZINTFMONITOR012 .
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
  MODIFY ZINTFMONITOR012  FROM TABLE it_list.

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
