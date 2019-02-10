"! <p class="shorttext synchronized" lang="en">Interface Parameters descriptions</p>
class ZCL_ZINTFMONITOR013_READ definition
  public
  create private INHERITING FROM zcl_zintfmonitor_base_read..

public section.

  "! <p class="shorttext synchronized" lang="en">Delete Details</p>
  "!
  "! @parameter is_details | <p class="shorttext synchronized" lang="en">List Details</p>
  class-methods DELETE_DETAILS
    importing
      !IS_DETAILS type ZINTFMONITOR013
    raising
      zcx_intfmonitor .
  "! <p class="shorttext synchronized" lang="en">Delete Multiple</p>
  "!
  "! @parameter it_list | <p class="shorttext synchronized" lang="en">List Details</p>
  class-methods DELETE_LIST
    importing
      !IT_LIST type ZTT_ZINTFMONITOR013
    raising
      zcx_intfmonitor .
  "! <p class="shorttext synchronized" lang="en">Find details by keys</p>
  "!
  "! @parameter id_intfid | <p class="shorttext synchronized" lang="en">Interface</p>
  "! @parameter id_spras  | <p class="shorttext synchronized" lang="en">Language</p>
  "! @parameter id_param  | <p class="shorttext synchronized" lang="en">Parameter</p>
  "! @parameter rs_result | <p class="shorttext synchronized" lang="en">Details</p>
  class-methods GET_DETAILS
    importing
      !ID_INTFID type ZINTFMONITOR013-INTFID
      !ID_SPRAS type ZINTFMONITOR013-SPRAS
      !ID_PARAM type ZINTFMONITOR013-PARAM
    returning
      value(RS_RESULT) type ZINTFMONITOR013
    raising
      zcx_intfmonitor .
  "! <p class="shorttext synchronized" lang="en">Find Multiple details by keys</p>
  "!
  "! @parameter id_intfid | <p class="shorttext synchronized" lang="en">Interface</p>
  "! @parameter id_spras  | <p class="shorttext synchronized" lang="en">Language</p>
  "! @parameter id_param  | <p class="shorttext synchronized" lang="en">Parameter</p>
  "! @parameter et_list   | <p class="shorttext synchronized" lang="en">List Details</p>
  class-methods GET_LIST
    importing
      !ID_INTFID type ZINTFMONITOR013-INTFID optional
      !ID_SPRAS type ZINTFMONITOR013-SPRAS optional
      !ID_PARAM type ZINTFMONITOR013-PARAM optional
    exporting
      !ET_LIST type ZTT_ZINTFMONITOR013
    raising
      zcx_intfmonitor .
  "! <p class="shorttext synchronized" lang="en">Initializes Buffer Data</p>
  class-methods INIT_BUFFER .
  "! <p class="shorttext synchronized" lang="en">Save Details</p>
  "!
  "! @parameter is_details | <p class="shorttext synchronized" lang="en">List Details</p>
  class-methods SAVE_DETAILS
    importing
      !IS_DETAILS type ZINTFMONITOR013
    raising
      zcx_intfmonitor .
  "! <p class="shorttext synchronized" lang="en">Save Multiple</p>
  "!
  "! @parameter it_list | <p class="shorttext synchronized" lang="en">List Details</p>
  class-methods SAVE_LIST
    importing
      !IT_LIST type ZTT_ZINTFMONITOR013
    raising
      zcx_intfmonitor .
protected section.
private section.

  types:
     BEGIN OF mtyp_ranges,
 INTFID TYPE RANGE OF ZINTFMONITOR013-INTFID ,
 SPRAS TYPE RANGE OF ZINTFMONITOR013-SPRAS ,
 PARAM TYPE RANGE OF ZINTFMONITOR013-PARAM ,
        END   OF mtyp_ranges .

  "! <p class="shorttext synchronized" lang="en">Selection Ranges</p>
  class-data MS_RANGES type MTYP_RANGES .
  "! <p class="shorttext synchronized" lang="en">Data Buffer</p>
  class-data MT_BUFFER type ZTT_ZINTFMONITOR013 .

ENDCLASS.



CLASS ZCL_ZINTFMONITOR013_READ IMPLEMENTATION.


  method DELETE_DETAILS.
  DATA lt_list      TYPE ZTT_ZINTFMONITOR013 .
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
  DELETE ZINTFMONITOR013  FROM TABLE it_list.

  IF NOT sy-subrc IS INITIAL.
    RAISE EXCEPTION TYPE zcx_intfmonitor.
  ENDIF.
  endmethod.


  method GET_DETAILS.

  DATA lt_list      TYPE ZTT_ZINTFMONITOR013 .
  DATA ls_list      LIKE LINE OF lt_list.
  DATA lo_exception TYPE REF TO zcx_intfmonitor.

  READ TABLE mt_buffer INTO rs_result
    WITH KEY INTFID = ID_INTFID
						 SPRAS = ID_SPRAS
						 PARAM = ID_PARAM.

  IF NOT sy-subrc IS INITIAL.

    TRY.
        get_list( EXPORTING  ID_INTFID = ID_INTFID
														 ID_SPRAS = ID_SPRAS
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

  IF ID_SPRAS IS SUPPLIED.
    _add_range( EXPORTING id_low   = ID_SPRAS
                CHANGING  ct_range = ms_ranges-SPRAS ).
  ENDIF.

  IF ID_PARAM IS SUPPLIED.
    _add_range( EXPORTING id_low   = ID_PARAM
                CHANGING  ct_range = ms_ranges-PARAM ).
  ENDIF.

SELECT * INTO TABLE et_list
  FROM ZINTFMONITOR013
  WHERE INTFID IN ms_ranges-INTFID
    AND SPRAS IN ms_ranges-SPRAS
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
  DATA lt_list type ZTT_ZINTFMONITOR013 .
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
  MODIFY ZINTFMONITOR013  FROM TABLE it_list.

  IF NOT sy-subrc IS INITIAL.
    RAISE EXCEPTION TYPE zcx_intfmonitor.
  ENDIF.
  endmethod.



ENDCLASS.
