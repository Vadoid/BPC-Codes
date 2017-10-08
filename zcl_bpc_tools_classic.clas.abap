class ZCL_BPC_TOOLS_CLASSIC definition
  public
  final
  create public .

public section.

  data I_APPSET_ID type UJ_APPSET_ID .
  data I_APPL_ID type UJ_APPL_ID .
  data I_DIMENSION_ID type UJ_DIM_NAME .
  data IT_CV type UJK_T_CV .
  data IT_PARAM type UJK_T_SCRIPT_LOGIC_HASHTABLE .

  methods ADD_COMMENTS
    importing
      !I_TABNAME type TABNAME
      !IT_COMPACT_CMTBL type UJC_T_COMPACT_CMTBL .
  methods COPY_COMMENTS
    importing
      !LV_CATEGORY_FROM type STRING
      !LV_CATEGORY_TO type STRING
      !LV_TIME_FROM type UJ_DIM_MEMBER optional
      !LV_TIME_TO type UJ_DIM_MEMBER optional
      !TIMESCOPE type SORTED TABLE
    exporting
      !ET_MESSAGE type UJ0_T_MESSAGE .
  methods CONSTRUCTOR
    importing
      !I_ENV_ID type UJ_APPSET_ID optional
      !I_MOD_ID type UJ_APPL_ID optional
      !I_DIM_ID type UJ_DIM_NAME optional
      !I_T_CV type UJK_T_CV optional
      !I_T_PARAM type UJK_T_SCRIPT_LOGIC_HASHTABLE optional .
  class-methods PERIOD_OFFSET
    importing
      !IMPORT_PERIOD type UJ_DIM_MEMBER
      !OFFSET type UJ_SDATA
    returning
      value(EXPORT_PERIOD) type UJ_DIM_MEMBER .
  methods TARGET_ACCS
    importing
      !IT_CV type UJK_T_CV
    exporting
      !ET_MESSAGE type UJ0_T_MESSAGE
      !TARGET_ACCOUNTS type HASHED TABLE
    raising
      CX_UJ_CUSTOM_LOGIC .
  methods READ_MODEL_DATA
    importing
      !READ_MODEL type UJ_APPL_ID optional
      !IT_CV type UJK_T_CV optional
      !IT_SEL type UJ0_T_SEL optional
    exporting
      value(OUTPUT_DATA) type STANDARD TABLE .
  methods READ_MODEL_DATA_AXIS
    importing
      !READ_MODEL type UJ_APPL_ID optional
      !IT_CV type UJK_T_CV optional
      !IT_SEL type UJ0_T_SEL optional
    exporting
      value(OUTPUT_DATA) type STANDARD TABLE .
  methods GET_INACTIVE_ENTITY
    importing
      !GET_EXTRA_DIM type CHAR1 optional
    returning
      value(OUTPUT_R_DATA) type ref to DATA .
  methods GET_PARENT_MD
    importing
      !PARENT_MBR type UJ_DIM_MEMBER
      !I_DIMENSION_ID type UJ_DIM_NAME optional
    returning
      value(OUTPUT_DATA) type UJA_T_DIM_MEMBER .
  methods GET_SNAPSHOT_ID
    importing
      !CURRENT_PERIOD type UJ_DIM_MEMBER
    returning
      value(FCST_SNAPSHOT) type STRING .
  methods TIME
    importing
      !CATEGORY type STRING optional
      !EXTEND type CHAR1 optional
    exporting
      !TIMEQRTLY type SORTED TABLE
      !TIMESCOPE type SORTED TABLE
      !FIRST_PERIOD type UJ_DIM_MEMBER
      !LAST_PERIOD type UJ_DIM_MEMBER
      !CURRENT_PERIOD type UJ_DIM_MEMBER .
  methods WRITE_MODEL_DATA
    importing
      !TARGET_MODEL type UJ_APPL_ID optional
      !INPUT_DATA type STANDARD TABLE optional
      !WRITE type CHAR1
    exporting
      !LT_DIM_LIST type UJA_T_DIM_LIST
      !ET_MESSAGE type UJ0_T_MESSAGE
      !ET_ERROR_RECORDS type STANDARD TABLE
      !LR_DATA type ref to DATA .
  methods GET_MD_FROM_CONTEXT
    importing
      !I_DIMENSION_ID type UJ_DIM_NAME
    changing
      !LT_SEL type UJ0_T_SEL optional
    returning
      value(OUTPUT_R_DATA) type ref to DATA .
  methods READ_MASTER_DATA
    importing
      !I_DIMENSION_ID type UJ_DIM_NAME
      !PROPERTY_ID type STRING optional
      !PROPERTY_VALUE type STRING optional
    exporting
      !OUTPUT_R_DATA type ref to DATA
    changing
      !LT_SEL type UJ0_T_SEL optional .
  class-methods READ_MASTER_DATA_HIERARCHY
    importing
      !I_APPSET_ID type UJ_APPSET_ID
      !I_DIMENSION type UJ_DIM_NAME
      !I_PARENT_MBR type UJ_DIM_MEMBER
    exporting
      !OUTPUT_DATA type STANDARD TABLE .
  class-methods READ_ACC_TRANS_RULES
    importing
      !I_APPSET_ID type UJ_APPSET_ID
      !I_APPL_ID type UJ_APPL_ID
      !TVARVC_NAME type RVARI_VNAM
    exporting
      !ET_MESSAGE type UJ0_T_MESSAGE
      !OUTPUT_DATA type HASHED TABLE .
  class-methods READ_ACC_TRANS_RULESV2
    importing
      !I_APPSET_ID type UJ_APPSET_ID
      !I_APPL_ID type UJ_APPL_ID
      !TVARVC_NAME type RVARI_VNAM
      !EXPAND type CHAR1 optional
    exporting
      !ET_MESSAGE type UJ0_T_MESSAGE
      !OUTPUT_DATA type HASHED TABLE .
  methods COPY_COMMENTS_RS
    importing
      !LT_DIMLIST_OUT type UJC_T_CMTBL_DIMLIST optional
      !LT_DIMLIST_INP type UJC_T_CMTBL_DIMLIST optional
      !NEW_ENTITY type STRING optional
    exporting
      !ET_MESSAGE type UJ0_T_MESSAGE .
  PROTECTED SECTION.
private section.

  methods GET_COMMENTS_TABLE
    returning
      value(E_TABNAME) type TABNAME .
ENDCLASS.



CLASS ZCL_BPC_TOOLS_CLASSIC IMPLEMENTATION.


  method ADD_COMMENTS.

      data:

        l_recordid              type uj0_uni_idc,         " auto-generated comment id GUID
        l_userid                type uj_user_id,          " userid by context, for security
        l_datewritten           type timestamp,           " auto-generated timestamp
        lr_s_expand_cmtbl       type ref to data,         " expanded structure of comments table
        lr_t_expand_cmtbl       type ref to data,         " itab of expanded comments table
        lox_ex_handler          type ref to cx_root,      " exception handler
        if_time                 TYPE uj_flg VALUE abap_false,
        rf_inserted             TYPE uj_flg.

  field-symbols: <ls_cmt_tbl>   type any,                 " expanded structure of comments table
                 <lt_cmt_tbl>   type standard table,      " itab of expanded comements table
                 <l_dim_member> type any.                 " loop dynamic dimension member

  field-symbols:
                 <ls_compact_cmtbl>        type ujc_s_compact_cmtbl, " compact comment table structure
                 <ls_dim_member> type ujc_s_cmtbl_dimlist, " dimension (name,value) pair
                 <l_recordid>    type any,                " structure field : guid
                 <l_userid>      type any,                " structure field : originator
                 <l_datewritten> type any,                " structure field : timestamp
                 <l_keyword>     type any,                " structure field : keyword
                 <l_priority>    type any,                " structure field : priority
                 <l_kpi>         type any,                " structure field : kpi
                 <l_measures>    type any,                " structure field : measures
                 <l_scomment>    type any.                " structure field : comment

  " create structure of comments table
  create data lr_s_expand_cmtbl type (i_tabname).
  assign lr_s_expand_cmtbl->* to <ls_cmt_tbl>.
  assert sy-subrc = 0.
* Internal Error!
* Assertion Failure probably due to inexist DDIC table name.
*  IF <ls_cmt_tbl> IS NOT ASSIGNED.
*    RAISE EXCEPTION TYPE cx_ujc_exception
*      EXPORTING
*        textid = cx_ujc_exception=>ex_cannot_assign_fs.
*  ENDIF.

  " create itab of comments table
  create data lr_t_expand_cmtbl like standard table of <ls_cmt_tbl>.
  assign lr_t_expand_cmtbl->* to <lt_cmt_tbl>.
  assert sy-subrc = 0.
* Internal Error!
* Assertion Failure probably due to inexist DDIC table name.
*  IF <lt_cmt_tbl> IS NOT ASSIGNED.
*    RAISE EXCEPTION TYPE cx_ujc_exception
*      EXPORTING
*        textid = cx_ujc_exception=>ex_cannot_assign_fs.
*  ENDIF.

  assign component ujc0_cs_cmtbl_fieldname-recordid    of structure <ls_cmt_tbl> to <l_recordid>.
  assert sy-subrc = 0.
  assign component ujc0_cs_cmtbl_fieldname-userid      of structure <ls_cmt_tbl> to <l_userid>.
  assert sy-subrc = 0.
  assign component ujc0_cs_cmtbl_fieldname-datewritten of structure <ls_cmt_tbl> to <l_datewritten>.
  assert sy-subrc = 0.
  assign component ujc0_cs_cmtbl_fieldname-keyword     of structure <ls_cmt_tbl> to <l_keyword>.
  assert sy-subrc = 0.
  assign component ujc0_cs_cmtbl_fieldname-priority    of structure <ls_cmt_tbl> to <l_priority>.
  assert sy-subrc = 0.
  assign component ujc0_cs_cmtbl_fieldname-kpi         of structure <ls_cmt_tbl> to <l_kpi>.
  assert sy-subrc = 0.
  assign component ujc0_cs_cmtbl_fieldname-measures    of structure <ls_cmt_tbl> to <l_measures>.
  assert sy-subrc = 0.
  assign component ujc0_cs_cmtbl_fieldname-scomment    of structure <ls_cmt_tbl> to <l_scomment>.
  assert sy-subrc = 0.

  " set UserID
*  l_userid = do_context->ds_user-user_id.

  " for each comment, expand structure, rebuild expanded itab
  " compact itab => compact struct => expand struct => expand itab
  loop at it_compact_cmtbl assigning <ls_compact_cmtbl>.

    clear <ls_cmt_tbl>.

    " ===================
    " auto-generated data
    " ===================
    " set RecordID
    cl_uj_services=>generate_guid( importing e_uni_idc = l_recordid ).
    " set timestamp


    if if_time eq abap_true.
      " timestamp
      get time stamp field l_datewritten.
    else.
      l_datewritten = <ls_compact_cmtbl>-datewritten.
    endif.


    " ==================================================
    " fill in comments table structure with comment data
    " assign field value one by one
    " ==================================================
    " recordid
    <l_recordid> = l_recordid.

    " userid
    <l_userid> = l_userid.

    " timestamp
    <l_datewritten> = l_datewritten.

    " keyword
    <l_keyword> = <ls_compact_cmtbl>-keyword.

    " priority
    if <ls_compact_cmtbl>-priority is not initial.
      <l_priority> = <ls_compact_cmtbl>-priority.
    else.
      <l_priority> = ujc0_cs_cmt_priority-none.
    endif.

    " kpi
    <l_kpi> = <ls_compact_cmtbl>-kpi.

    " dimensions
    loop at <ls_compact_cmtbl>-dim_list assigning <ls_dim_member>.

      assign component <ls_dim_member>-dim_name of structure <ls_cmt_tbl> to <l_dim_member>.

      check sy-subrc = 0.
*      if <ls_dim_member>-dim_value = ujc0_c_nonemember.
*        <l_dim_member> = ''.
*      else.
        <l_dim_member> = <ls_dim_member>-dim_value.
*      endif.

    endloop.

    " measures
    <l_measures> = <ls_compact_cmtbl>-measures.

    " comment
    <l_scomment> = <ls_compact_cmtbl>-scomment.

    " =========================
    " insert into expanded itab
    " =========================
    append <ls_cmt_tbl> to <lt_cmt_tbl>.

  endloop.


  " ==================================
  " insert comment into comments table
  " ==================================
  try.
      insert (i_tabname)
        from table <lt_cmt_tbl>.

      if sy-subrc = 0.
        rf_inserted = abap_true.
      else.
        rf_inserted = abap_false.
      endif.

    catch cx_dynamic_check into lox_ex_handler.
      raise exception type cx_ujc_exception
        exporting
          previous = lox_ex_handler
          textid   = cx_ujc_exception=>ex_dyn_sql_err.
  endtry.

endmethod.


  method CONSTRUCTOR.
*&---------------------------------------------------------------------*
*&  Class            ZCL_BPC_TOOLS_CLASSIC
*&  Method           CONSTRUCTOR
*&---------------------------------------------------------------------*
****************************************************************************
* Program type....:  Customer Custom Method
* Author...........: VZ
* Date.............: February 2017
* ----------------------------------------------------------------------
* Description......: The class handles some service operations to support
*                    Customer Planning project.
*
****************************************************************************
* Change Log
****************************************************************************
* Changed on:                 By:
* Description:
****************************************************************************

i_appset_id = i_env_id.
i_appl_id = i_mod_id.
i_dimension_id = i_dim_id.
it_param = i_t_param.
it_cv = i_t_cv.



  endmethod.


  method COPY_COMMENTS.


    DATA: ls_param       TYPE ujk_s_script_logic_hashentry,
          ls_time TYPE UJ_DIM_MEMBER,
          lv_project     TYPE  uj_dim_member,
          lv_tabname     TYPE  tabname,
          cs_sqmark      TYPE char1 VALUE '"',
          lt_projects    TYPE TABLE OF string,
          ls_projects    TYPE string,
          lo_ex          TYPE REF TO cx_uj_custom_logic,
          ls_message     TYPE uj0_s_message,
          lt_message     TYPE uj0_t_message,
          ls_recordid    TYPE uj0_uni_idc,
          lt_recordid    TYPE ujc_t_cmt_recordid.

    DATA: lo_cmt_manager TYPE REF TO cl_ujc_cmtmanager,
          lt_dimlist     TYPE ujc_t_cmtbl_dimlist,
          ls_dimlist     TYPE ujc_s_cmtbl_dimlist,
          lt_comments    TYPE ujc_t_compact_cmtbl,
          ls_comments    TYPE ujc_s_compact_cmtbl,
          ef_success     TYPE flag,
          lv_tabix       TYPE i,
          lv_ctabix      TYPE i,
          et_error_cmtbl TYPE ujc_t_compact_cmtbl.



    DATA: BEGIN OF ls_commkey,
            mandt    TYPE mandt,
            recordid TYPE uj0_uni_idc,
          END OF ls_commkey.

    DATA: lt_commkey LIKE TABLE OF ls_commkey.

        TRY.
            " Create Comment Manager

            CREATE OBJECT lo_cmt_manager
              EXPORTING
                i_appset_id = i_appset_id
                i_appl_id   = i_appl_id.

          CATCH cx_ujc_exception.
            ls_message-msgty = 'E'.
            ls_message-msgv1 = i_appl_id.
            ls_message-message = 'Cannot get handle to comments for model '.
            APPEND ls_message TO et_message.
            RAISE EXCEPTION TYPE cx_uj_custom_logic.
        ENDTRY.

******Read Destination Comments****************

        ls_dimlist-dim_name = 'CATEGORY' .
        ls_dimlist-dim_value = lv_category_to.
        APPEND ls_dimlist TO lt_dimlist.



        ls_dimlist-dim_name = 'TIME' .

        IF LV_TIME_TO IS NOT INITIAL.
        ls_dimlist-dim_value = LV_TIME_TO.
        APPEND ls_dimlist TO lt_dimlist.

        ELSE.

        LOOP AT TIMESCOPE INTO LS_TIME.
        ls_dimlist-dim_value = LS_TIME.
        APPEND ls_dimlist TO lt_dimlist.
        ENDLOOP.

        ENDIF.


        TRY.
            IF lt_dimlist[] IS NOT INITIAL.
              CALL METHOD lo_cmt_manager->get_cmt
                EXPORTING
*                 i_keyword         = i_keyword
*                 it_priority       = lt_priority
                  it_dim_members    = lt_dimlist
*                 i_measures        = i_measures
*                 i_originator      = i_originator
*                 i_cmt_date_from   = i_cmt_date_from
*                 i_cmt_date_to     = i_cmt_date_to
                  if_with_history   = abap_true
                  if_search_keyword = abap_true
                IMPORTING
                  et_compact_cmtbl  = lt_comments
                CHANGING
                  ct_message        = et_message.
            ENDIF.
          CATCH cx_ujc_exception.

            ls_message-msgty = 'E'.
            ls_message-msgv1 = i_appl_id.
            ls_message-message = 'No Comments retrieved for model '.
            APPEND ls_message TO et_message.
        "    RAISE EXCEPTION TYPE cx_uj_custom_logic.

       ENDTRY.


        LOOP AT lt_comments INTO ls_comments.
          ls_commkey-mandt = sy-mandt.
          ls_commkey-recordid = ls_comments-recordid.
          APPEND ls_commkey TO lt_commkey.
        ENDLOOP.

lv_tabname = me->GET_COMMENTS_TABLE( ). "Get table name.


        TRY.
            IF lt_commkey[] IS NOT INITIAL.


    "          lv_tabname = me->GET_COMMENTS_TABLE( ). " This should be outisde lt_commkey clause. We need table ID in any case!

              DELETE (lv_tabname) FROM TABLE @lt_commkey.

            ENDIF.
          CATCH cx_ujc_exception .
            APPEND LINES OF lt_message TO et_message.
            RAISE EXCEPTION TYPE cx_uj_custom_logic.
        ENDTRY.

******Read Destination Comments****************

******Read Source Comments****************

        CLEAR lt_dimlist[].
        ls_dimlist-dim_name = 'CATEGORY' .
        ls_dimlist-dim_value = lv_category_from.
        APPEND ls_dimlist TO lt_dimlist.


        IF LV_TIME_FROM IS NOT INITIAL.
        ls_dimlist-dim_value = LV_TIME_FROM.
        APPEND ls_dimlist TO lt_dimlist.

        ELSE.

        LOOP AT TIMESCOPE INTO LS_TIME.
        ls_dimlist-dim_value = LS_TIME.
        APPEND ls_dimlist TO lt_dimlist.
        ENDLOOP.

        ENDIF.

        TRY.
            IF lt_dimlist[] IS NOT INITIAL.
              CALL METHOD lo_cmt_manager->get_cmt
                EXPORTING
*                 i_keyword         = i_keyword
*                 it_priority       = lt_priority
                  it_dim_members    = lt_dimlist
*                 i_measures        = i_measures
*                 i_originator      = i_originator
*                 i_cmt_date_from   = i_cmt_date_from
*                 i_cmt_date_to     = i_cmt_date_to
                  if_with_history   = abap_false
                  if_search_keyword = abap_true
                IMPORTING
                  et_compact_cmtbl  = lt_comments
                CHANGING
                  ct_message        = et_message.
            ENDIF.
          CATCH cx_ujc_exception.
            ls_message-msgty = 'E'.
            ls_message-msgv1 = i_appl_id.
            ls_message-message = 'No Comments retrieved for model '.
            APPEND ls_message TO et_message.
            RAISE EXCEPTION TYPE cx_uj_custom_logic.
        ENDTRY.
******Read Source Comments****************

        DELETE lt_comments WHERE scomment = '...'.
        DELETE lt_comments WHERE scomment = space.

        LOOP AT lt_comments INTO ls_comments.
          lv_ctabix = sy-tabix.
          LOOP AT ls_comments-dim_list INTO ls_dimlist.
            lv_tabix = sy-tabix.
            IF ls_dimlist-dim_name = 'CATEGORY' .
              ls_dimlist-dim_value = lv_category_to .
              MODIFY ls_comments-dim_list FROM ls_dimlist INDEX lv_tabix.
            ENDIF.

      IF ls_dimlist-dim_name = 'TIME' AND lv_time_to IS NOT INITIAL.
        ls_dimlist-dim_value = lv_time_to .
        MODIFY ls_comments-dim_list FROM ls_dimlist INDEX lv_tabix.
      ENDIF.

          ENDLOOP.
          MODIFY lt_comments FROM ls_comments INDEX lv_ctabix.

        ENDLOOP.

*      Insert the data into table
        TRY.
          me->add_comments(
               i_tabname        = lv_tabname
               it_compact_cmtbl = lt_comments
               ).

         " CATCH cx_ujc_exception INTO lox_ex_handler.
        ENDTRY.
*        TRY.
*
*            CALL METHOD lo_cmt_manager->add_cmt
*              EXPORTING
*                it_compact_cmtbl = lt_comments
*              IMPORTING
*                ef_inserted      = ef_success
*                et_error_cmtbl   = et_error_cmtbl
*              CHANGING
*                ct_message       = et_message.
*
*          CATCH cx_ujc_exception.
*            ls_message-msgty = 'E'.
*            ls_message-msgv1 = i_appl_id.
*            ls_message-message = 'Error writing comments to model '.
*            APPEND ls_message TO et_message.
*            RAISE EXCEPTION TYPE cx_uj_custom_logic.
*
*        ENDTRY.

*      CATCH cx_uj_custom_logic INTO lo_ex.
*        lo_ex->messages = et_message.

*    ENDTRY.




  ENDMETHOD.


  METHOD COPY_COMMENTS_RS.


    DATA: LS_PARAM       TYPE UJK_S_SCRIPT_LOGIC_HASHENTRY,
          LS_TIME TYPE UJ_DIM_MEMBER,
          LV_PROJECT     TYPE  UJ_DIM_MEMBER,
          LV_TABNAME     TYPE  TABNAME,
          CS_SQMARK      TYPE CHAR1 VALUE '"',
          LT_PROJECTS    TYPE TABLE OF STRING,
          LS_PROJECTS    TYPE STRING,
          LO_EX          TYPE REF TO CX_UJ_CUSTOM_LOGIC,
          LS_MESSAGE     TYPE UJ0_S_MESSAGE,
          LT_MESSAGE     TYPE UJ0_T_MESSAGE,
          LS_RECORDID    TYPE UJ0_UNI_IDC,
          LT_RECORDID    TYPE UJC_T_CMT_RECORDID.

    DATA: LO_CMT_MANAGER TYPE REF TO CL_UJC_CMTMANAGER,
          LT_DIMLIST     TYPE UJC_T_CMTBL_DIMLIST,
          LS_DIMLIST     TYPE UJC_S_CMTBL_DIMLIST,
          LT_COMMENTS    TYPE UJC_T_COMPACT_CMTBL,
          LS_COMMENTS    TYPE UJC_S_COMPACT_CMTBL,
          EF_SUCCESS     TYPE FLAG,
          LV_TABIX       TYPE I,
          LV_CTABIX      TYPE I,
          ET_ERROR_CMTBL TYPE UJC_T_COMPACT_CMTBL.


    DATA: BEGIN OF LS_COMMKEY,
            MANDT    TYPE MANDT,
            RECORDID TYPE UJ0_UNI_IDC,
          END OF LS_COMMKEY.

    DATA: LT_COMMKEY LIKE TABLE OF LS_COMMKEY.

        TRY.
            " Create Comment Manager

            CREATE OBJECT LO_CMT_MANAGER
              EXPORTING
                I_APPSET_ID = I_APPSET_ID
                I_APPL_ID   = I_APPL_ID.

          CATCH CX_UJC_EXCEPTION.
            LS_MESSAGE-MSGTY = 'E'.
            LS_MESSAGE-MSGV1 = I_APPL_ID.
            LS_MESSAGE-MESSAGE = 'Cannot get handle to comments for model '.
            APPEND LS_MESSAGE TO ET_MESSAGE.
            RAISE EXCEPTION TYPE CX_UJ_CUSTOM_LOGIC.
        ENDTRY.

******Read Destination Comments****************START

        CLEAR LT_DIMLIST.
        LT_DIMLIST = LT_DIMLIST_OUT."INTERSECTION WE WANT TO WRITE TO REMOVE

        TRY.
            IF LT_DIMLIST[] IS NOT INITIAL.
              CALL METHOD LO_CMT_MANAGER->GET_CMT
                EXPORTING
*                 i_keyword         = i_keyword
*                 it_priority       = lt_priority
                  IT_DIM_MEMBERS    = LT_DIMLIST
*                 i_measures        = i_measures
*                 i_originator      = i_originator
*                 i_cmt_date_from   = i_cmt_date_from
*                 i_cmt_date_to     = i_cmt_date_to
                  IF_WITH_HISTORY   = ABAP_TRUE
                  IF_SEARCH_KEYWORD = ABAP_TRUE
                IMPORTING
                  ET_COMPACT_CMTBL  = LT_COMMENTS
                CHANGING
                  CT_MESSAGE        = ET_MESSAGE.
            ENDIF.
          CATCH CX_UJC_EXCEPTION.

            LS_MESSAGE-MSGTY = 'E'.
            LS_MESSAGE-MSGV1 = I_APPL_ID.
            LS_MESSAGE-MESSAGE = 'No Comments retrieved for model '.
            APPEND LS_MESSAGE TO ET_MESSAGE.
        "    RAISE EXCEPTION TYPE cx_uj_custom_logic.
       ENDTRY.

        LOOP AT LT_COMMENTS INTO LS_COMMENTS.
          LS_COMMKEY-MANDT = SY-MANDT.
          LS_COMMKEY-RECORDID = LS_COMMENTS-RECORDID.
          APPEND LS_COMMKEY TO LT_COMMKEY.
        ENDLOOP.

        LV_TABNAME = ME->GET_COMMENTS_TABLE( ). "Get table name.

        IF LT_COMMKEY[] IS NOT INITIAL.
          DELETE (LV_TABNAME) FROM TABLE @LT_COMMKEY."DELETE SINCE THERE IS NO OVERWRITE FUNCTION
        ENDIF.

******Read Destination Comments****************END


******Read Source Comments****************START
        CLEAR LT_DIMLIST.
        LT_DIMLIST = LT_DIMLIST_INP.
        "INTERSECTION WE WANT TO COPY BUT REPLACE WITH NEW_ENTITY

        TRY.
            IF LT_DIMLIST[] IS NOT INITIAL.
              CALL METHOD LO_CMT_MANAGER->GET_CMT
                EXPORTING
*                 i_keyword         = i_keyword
*                 it_priority       = lt_priority
                  IT_DIM_MEMBERS    = LT_DIMLIST
*                 i_measures        = i_measures
*                 i_originator      = i_originator
*                 i_cmt_date_from   = i_cmt_date_from
*                 i_cmt_date_to     = i_cmt_date_to
                  IF_WITH_HISTORY   = ABAP_FALSE
                  IF_SEARCH_KEYWORD = ABAP_TRUE
                IMPORTING
                  ET_COMPACT_CMTBL  = LT_COMMENTS
                CHANGING
                  CT_MESSAGE        = ET_MESSAGE.
            ENDIF.
          CATCH CX_UJC_EXCEPTION.
            LS_MESSAGE-MSGTY = 'E'.
            LS_MESSAGE-MSGV1 = I_APPL_ID.
            LS_MESSAGE-MESSAGE = 'No Comments retrieved for model '.
            APPEND LS_MESSAGE TO ET_MESSAGE.
           " RAISE EXCEPTION TYPE CX_UJ_CUSTOM_LOGIC.
        ENDTRY.
******Read Source Comments****************END


IF LT_COMMENTS IS NOT INITIAL.

        DELETE LT_COMMENTS WHERE SCOMMENT = '...'.
        DELETE LT_COMMENTS WHERE SCOMMENT = SPACE.

        LOOP AT LT_COMMENTS INTO LS_COMMENTS.
          LV_CTABIX = SY-TABIX.
          LOOP AT LS_COMMENTS-DIM_LIST INTO LS_DIMLIST.
            LV_TABIX = SY-TABIX.
            IF LS_DIMLIST-DIM_NAME = 'ENTITY' .
              LS_DIMLIST-DIM_VALUE = NEW_ENTITY .
              MODIFY LS_COMMENTS-DIM_LIST FROM LS_DIMLIST INDEX LV_TABIX.
            ENDIF.

          ENDLOOP.
          MODIFY LT_COMMENTS FROM LS_COMMENTS INDEX LV_CTABIX.

        ENDLOOP.

*      Insert the data into table
        TRY.
          ME->ADD_COMMENTS(
               I_TABNAME        = LV_TABNAME
               IT_COMPACT_CMTBL = LT_COMMENTS
               ).

         " CATCH cx_ujc_exception INTO lox_ex_handler.
        ENDTRY.
ENDIF.

  ENDMETHOD.


  METHOD GET_COMMENTS_TABLE.

* DATA: infocube type string,
*       comments_table type string,
*       comments_archive_table type string.
*
* "Get Infocube name
*
* SELECT SINGLE INFOCUBE FROM UJA_APPL INTO infocube
*   WHERE APPSET_ID = i_appset_id
*   and application_id = i_appl_id.
*
*comments_table = '/1CPMB/' && infocube+6(2) && infocube+9(3) && 'CMT'.

    DATA: l_prefix       TYPE uja_s_prefix,           " prefix of (appset,appl)
          lo_gentab      TYPE REF TO cl_uj_gen_table, " Singleton instance
          lox_ex_handler TYPE REF TO cx_uj_static_check. " exception handler



    SELECT SINGLE appset_prefix FROM uja_appset_info INTO l_prefix-appset_prefix
                         WHERE appset_id = i_appset_id.

    SELECT SINGLE appl_prefix FROM uja_appl INTO l_prefix-appl_prefix
                       WHERE  appset_id = i_appset_id AND
                              application_id = i_appl_id.



    " get Singleton instance
    CALL METHOD cl_uj_gen_table=>get_instance
      IMPORTING
        eo_instance = lo_gentab.

    " get full table name using prefix + table type
    TRY.

        CALL METHOD lo_gentab->get_ddic_table_name
          EXPORTING
            i_table   = 'COMMENT'
            is_prefix = l_prefix
          IMPORTING
            e_tabname = e_tabname.
*            e_gotstate = e_gotstate.

      CATCH cx_uj_gen_ddic_error INTO lox_ex_handler.
    ENDTRY.

  ENDMETHOD.


  METHOD GET_INACTIVE_ENTITY.

    INCLUDE ZINC_PLAN_BADI_COMMON_VARS. "Common Variables

*** Properties for closed/inactive entities per model
    " MODEL_STATUS FORMAT

* Check entity type dimension from the instance Model

***********************GET ENTITY DIM *******************************
    LO_APPL_MGR = CL_UJA_BPC_ADMIN_FACTORY=>GET_APPLICATION_MANAGER(
     I_APPSET_ID = I_APPSET_ID
     I_APPLICATION_ID = I_APPL_ID ).
    CLEAR LS_APPLICATION.
    LO_APPL_MGR->GET(
     EXPORTING
     IF_WITH_MEASURES = ABAP_FALSE " BPC: Generic indicator
     IF_SUMMARY = ABAP_FALSE " BPC: Generic indicator
     IMPORTING
     ES_APPLICATION = LS_APPLICATION ). " Applications table type

    LOOP AT LS_APPLICATION-DIMENSIONS INTO LS_DIMENSIONS.
      IF LS_DIMENSIONS-DIM_TYPE = 'E'.
        ENTITY_DIM = LS_DIMENSIONS-DIMENSION.
      ENDIF.
    ENDLOOP.


    CLEAR: LT_SEL, LS_SEL.

    LS_SEL-DIMENSION = ENTITY_DIM.

    CASE I_APPL_ID.

    WHEN 'CASHFLOW'.

    LS_SEL-ATTRIBUTE = CASHFLOW_STATUS.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = '04'.
    APPEND LS_SEL TO LT_SEL.

    WHEN 'INC_STATEMENT' OR 'CAPEX'.

    LS_SEL-ATTRIBUTE = INC_STATEMENT_STATUS.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'BT'.
    LS_SEL-LOW = '03'.
    LS_SEL-HIGH = '10'.
    APPEND LS_SEL TO LT_SEL.

    WHEN 'RESOURCE'.

    IF GET_EXTRA_DIM = 'X'.

    ENTITY_DIM = 'EMPLOYEE'.
    LS_SEL-DIMENSION = ENTITY_DIM.
    LS_SEL-ATTRIBUTE = RESOURCE_STATUS.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'NE'.
    LS_SEL-LOW = '3'.
    APPEND LS_SEL TO LT_SEL.


    ELSE.
    LS_SEL-ATTRIBUTE = INC_STATEMENT_STATUS.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'BT'.
    LS_SEL-LOW = '03'.
    LS_SEL-HIGH = '10'.
    APPEND LS_SEL TO LT_SEL.
    ENDIF.

ENDCASE.

    CALL METHOD ME->READ_MASTER_DATA
      EXPORTING
        I_DIMENSION_ID = ENTITY_DIM
      IMPORTING
        OUTPUT_R_DATA  =  OUTPUT_R_DATA
      CHANGING
        LT_SEL         = LT_SEL.



**********************************************************************

  ENDMETHOD.


  METHOD GET_MD_FROM_CONTEXT.
*************************************************************************
*This method reads IT_CV and gets the master data for members in context*
*it also recieves LT_SEL (optional) if the selection has to be expanded)
*************************************************************************

    DATA: CV_DIMENSION    TYPE UJK_S_CV,
          LS_SEL            TYPE UJ0_S_SEL,
          WA_MEMBER   TYPE UJ_DIM_MEMBER.

     READ TABLE IT_CV INTO CV_DIMENSION WITH TABLE KEY DIM_UPPER_CASE = I_DIMENSION_ID.

     LOOP AT CV_DIMENSION-MEMBER INTO WA_MEMBER.
          LS_SEL-DIMENSION = I_DIMENSION_ID.
          LS_SEL-ATTRIBUTE = 'ID'.
          LS_SEL-SIGN = 'I'.
          LS_SEL-OPTION = 'EQ'.
          LS_SEL-LOW = WA_MEMBER.
          APPEND LS_SEL TO LT_SEL.

        ENDLOOP.

           ME->READ_MASTER_DATA(
          EXPORTING
           I_DIMENSION_ID   = I_DIMENSION_ID
          IMPORTING
            OUTPUT_R_DATA = OUTPUT_R_DATA ).

  ENDMETHOD.


  METHOD GET_PARENT_MD.

     CALL METHOD READ_MASTER_DATA_HIERARCHY
          EXPORTING
            I_APPSET_ID  = I_APPSET_ID
            I_DIMENSION  = I_DIMENSION_ID
            I_PARENT_MBR = PARENT_MBR
          IMPORTING
            OUTPUT_DATA  = OUTPUT_DATA.


  ENDMETHOD.


  METHOD GET_SNAPSHOT_ID.
    "Get Snapshot ID from Time DImension

    DATA: BEGIN OF LS_TIME,
            ID        TYPE CHAR32,
            COPYFORECAST TYPE CHAR32,
          END OF LS_TIME.

    DATA: LT_TIME LIKE STANDARD TABLE OF LS_TIME,
          MASTER_DATA TYPE REF TO DATA,
          LT_SEL      TYPE UJ0_T_SEL,
          LS_SEL      TYPE UJ0_S_SEL.

    FIELD-SYMBOLS: <DIMENSION_DATA> TYPE STANDARD TABLE.

    CLEAR: LT_SEL, LS_SEL.

    LS_SEL-DIMENSION = 'TIME'.
    LS_SEL-ATTRIBUTE = 'ID'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = CURRENT_PERIOD.
    APPEND LS_SEL TO LT_SEL.

    ME->READ_MASTER_DATA(
      EXPORTING
        I_DIMENSION_ID   =  LS_SEL-DIMENSION
      IMPORTING
        OUTPUT_R_DATA = MASTER_DATA
      CHANGING
        LT_SEL        = LT_SEL ).

    ASSIGN MASTER_DATA->* TO <DIMENSION_DATA>.
    MOVE-CORRESPONDING <DIMENSION_DATA> TO LT_TIME.

READ TABLE LT_TIME INTO LS_TIME WITH KEY ID = CURRENT_PERIOD.
FCST_SNAPSHOT = LS_TIME-COPYFORECAST.

  ENDMETHOD.


  METHOD PERIOD_OFFSET.
*offsets calendar year/period by the value provided
*includes positive and negative offsets

    DATA: SOURCE_MONTH    TYPE CHAR2,
          SOURCE_YEAR     TYPE CHAR4,
          OFFSET_NEGATIVE TYPE UJ_SDATA.

    SOURCE_YEAR = IMPORT_PERIOD(4).
    SOURCE_MONTH = IMPORT_PERIOD+5(2).

    IF OFFSET < 0. "include negative offset
      OFFSET_NEGATIVE = OFFSET * -1.
      DO OFFSET_NEGATIVE TIMES.
        SOURCE_MONTH = SOURCE_MONTH - 1.
        IF SOURCE_MONTH = 0.
          SOURCE_YEAR = SOURCE_YEAR - 1.
          SOURCE_MONTH = 12.
        ENDIF.
      ENDDO.
    ELSE.
      DO OFFSET TIMES.
        SOURCE_MONTH = SOURCE_MONTH + 1.
        IF SOURCE_MONTH = 13.
          SOURCE_YEAR = SOURCE_YEAR + 1.
          SOURCE_MONTH = 01.
        ENDIF.
      ENDDO.
    ENDIF.

    IF STRLEN( SOURCE_MONTH ) = 1.
      CONCATENATE SOURCE_YEAR '.0' SOURCE_MONTH INTO EXPORT_PERIOD.
    ELSE.
      CONCATENATE SOURCE_YEAR '.' SOURCE_MONTH INTO EXPORT_PERIOD.
    ENDIF.
  ENDMETHOD.


  METHOD READ_ACC_TRANS_RULES.
************************************ READ_ACC_TRANS_RULES START **********************************
******* SIMPLE SAMPLE READ OF ACCOUNT TRANSFORMATION RULES **********************************************

        DATA: LT_ACCOUNT_TR_RULES TYPE HASHED TABLE OF UJP_CALC_ACCOUNT WITH UNIQUE KEY APPSET_ID APPLICATION_ID CALC_ID SEQ,
              LT_ACCOUNT_TR_RULES_EXPAND TYPE HASHED TABLE OF UJP_CALC_ACCOUNT WITH UNIQUE KEY APPSET_ID APPLICATION_ID CALC_ID SEQ,
              LS_ACCOUNT_TR_RULES LIKE LINE OF LT_ACCOUNT_TR_RULES,
              LS_ACCOUNT_TR_RULES_EXPAND LIKE LINE OF LT_ACCOUNT_TR_RULES_EXPAND,
              L_LOG type string,
              ls_message like line of ET_MESSAGE,
              LS_TVARVC           TYPE TVARVC.
  DATA: ACCOUNTS            TYPE UJA_T_DIM_MEMBER,
        COUNTER TYPE UJ_SMALLINT,
        FLOWS            TYPE UJA_T_DIM_MEMBER,

          WA_MEMBER        TYPE UJ_DIM_MEMBER.



        SELECT SINGLE LOW FROM TVARVC INTO CORRESPONDING FIELDS OF LS_TVARVC WHERE NAME = TVARVC_NAME.
IF sy-subrc NE 0.
          CLEAR: ET_MESSAGE, L_LOG.

        CONCATENATE 'BUSINESS RULES ID FOR CASHFLOW SUBROUTINE 3 NOT FOUND - MAINTAIN TVARVC VARIBALE "'
        TVARVC_NAME '".' INTO L_LOG RESPECTING BLANKS.
        CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).

     ls_message-msgid = 'BPC' .
     ls_message-msgty = 'E'.
     ls_message-msgno = '010' .
     ls_message-message = L_LOG.
     APPEND ls_message TO et_message.

       EXIT.
ENDIF.
*********** GET RULES **********************************************
        SELECT APPSET_ID
          APPLICATION_ID
          CALC_ID
          SEQ
          ACCOUNT
          DEST_ACCOUNT
          SUBTABLES
          DEST_SUBTABLES
          INVERT_SIGN
          FROM UJP_CALC_ACCOUNT INTO CORRESPONDING FIELDS OF TABLE LT_ACCOUNT_TR_RULES WHERE CALC_ID = LS_TVARVC-LOW.
********************************************************************

CLEAR: LT_ACCOUNT_TR_RULES_EXPAND.

LOOP AT LT_ACCOUNT_TR_RULES INTO LS_ACCOUNT_TR_RULES.

  CLEAR: ACCOUNTS, FLOWS, WA_MEMBER, LS_ACCOUNT_TR_RULES_EXPAND.

CALL METHOD ZCL_BPC_TOOLS_CLASSIC=>READ_MASTER_DATA_HIERARCHY
  EXPORTING
    I_APPSET_ID  = I_APPSET_ID
    I_DIMENSION  = 'ACCOUNT'
    I_PARENT_MBR = LS_ACCOUNT_TR_RULES-ACCOUNT
  IMPORTING
   OUTPUT_DATA  = ACCOUNTS.

IF ACCOUNTS IS NOT INITIAL.

  READ TABLE LT_ACCOUNT_TR_RULES_EXPAND WITH KEY SEQ = LS_ACCOUNT_TR_RULES-SEQ TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
  COUNTER = COUNTER + 1.
  ELSE.
 COUNTER = LS_ACCOUNT_TR_RULES-SEQ.
 ENDIF.

  LOOP AT ACCOUNTS INTO WA_MEMBER.

    IF sy-tabix <> 1.
    COUNTER = COUNTER + 1.
    ENDIF.

    LS_ACCOUNT_TR_RULES_EXPAND =  LS_ACCOUNT_TR_RULES.
    LS_ACCOUNT_TR_RULES_EXPAND-ACCOUNT = WA_MEMBER.
    LS_ACCOUNT_TR_RULES_EXPAND-SEQ = COUNTER.



INSERT LS_ACCOUNT_TR_RULES_EXPAND INTO TABLE LT_ACCOUNT_TR_RULES_EXPAND.

ENDLOOP.

DELETE LT_ACCOUNT_TR_RULES WHERE ACCOUNT = LS_ACCOUNT_TR_RULES-ACCOUNT AND SEQ = LS_ACCOUNT_TR_RULES-SEQ.

ENDIF.

ENDLOOP.

INSERT LINES OF LT_ACCOUNT_TR_RULES_EXPAND INTO TABLE LT_ACCOUNT_TR_RULES.


***********************************************************************************************
CLEAR: LT_ACCOUNT_TR_RULES_EXPAND, LS_ACCOUNT_TR_RULES, COUNTER.

LOOP AT LT_ACCOUNT_TR_RULES INTO LS_ACCOUNT_TR_RULES.

  CLEAR: ACCOUNTS, FLOWS, WA_MEMBER, LS_ACCOUNT_TR_RULES_EXPAND.

CALL METHOD ZCL_BPC_TOOLS_CLASSIC=>READ_MASTER_DATA_HIERARCHY
  EXPORTING
    I_APPSET_ID  = I_APPSET_ID
    I_DIMENSION  = 'FLOW'
    I_PARENT_MBR = LS_ACCOUNT_TR_RULES-SUBTABLES
  IMPORTING
   OUTPUT_DATA  = FLOWS.

IF FLOWS IS NOT INITIAL.

  READ TABLE LT_ACCOUNT_TR_RULES_EXPAND WITH KEY SEQ = LS_ACCOUNT_TR_RULES-SEQ TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
  COUNTER = COUNTER + 1.
  ELSE.
 COUNTER = LS_ACCOUNT_TR_RULES-SEQ.
 ENDIF.

  LOOP AT FLOWS INTO WA_MEMBER.

    IF sy-tabix <> 1.
    COUNTER = COUNTER + 1.
    ENDIF.

    LS_ACCOUNT_TR_RULES_EXPAND =  LS_ACCOUNT_TR_RULES.
    LS_ACCOUNT_TR_RULES_EXPAND-SUBTABLES = WA_MEMBER.
    LS_ACCOUNT_TR_RULES_EXPAND-SEQ = COUNTER.



INSERT LS_ACCOUNT_TR_RULES_EXPAND INTO TABLE LT_ACCOUNT_TR_RULES_EXPAND.

ENDLOOP.

DELETE LT_ACCOUNT_TR_RULES WHERE ACCOUNT = LS_ACCOUNT_TR_RULES-ACCOUNT AND SEQ = LS_ACCOUNT_TR_RULES-SEQ AND SUBTABLES = LS_ACCOUNT_TR_RULES-SUBTABLES.

ENDIF.

ENDLOOP.

INSERT LINES OF LT_ACCOUNT_TR_RULES_EXPAND INTO TABLE LT_ACCOUNT_TR_RULES.

OUTPUT_DATA = LT_ACCOUNT_TR_RULES.


**********************************************************************************************************************
************************************ READ_ACC_TRANS_RULES END   **********************************
  ENDMETHOD.


  METHOD READ_ACC_TRANS_RULESV2.
************************************ READ_ACC_TRANS_RULES START **********************************
******* SIMPLE SAMPLE READ OF ACCOUNT TRANSFORMATION RULES **********************************************

        DATA: LT_ACCOUNT_TR_RULES TYPE HASHED TABLE OF UJP_CALC_ACCOUNT WITH UNIQUE KEY APPSET_ID APPLICATION_ID CALC_ID
          SEQ
          ACCOUNT
          DEST_ACCOUNT
          SUBTABLES
          DEST_SUBTABLES
          INVERT_SIGN,
              LT_ACCOUNT_TR_RULES_EXPAND TYPE HASHED TABLE OF UJP_CALC_ACCOUNT WITH UNIQUE KEY APPSET_ID APPLICATION_ID CALC_ID
          SEQ
          ACCOUNT
          DEST_ACCOUNT
          SUBTABLES
          DEST_SUBTABLES
          INVERT_SIGN,
              LS_ACCOUNT_TR_RULES LIKE LINE OF LT_ACCOUNT_TR_RULES,
              LS_ACCOUNT_TR_RULES_EXPAND LIKE LINE OF LT_ACCOUNT_TR_RULES_EXPAND,
              L_LOG type string,
              ls_message like line of ET_MESSAGE,
              LS_TVARVC           TYPE TVARVC.
  DATA: ACCOUNTS            TYPE UJA_T_DIM_MEMBER,
        COUNTER TYPE UJ_SMALLINT,
        FLOWS            TYPE UJA_T_DIM_MEMBER,

          WA_MEMBER        TYPE UJ_DIM_MEMBER.



        SELECT SINGLE LOW FROM TVARVC INTO CORRESPONDING FIELDS OF LS_TVARVC WHERE NAME = TVARVC_NAME.
IF sy-subrc NE 0.
          CLEAR: ET_MESSAGE, L_LOG.

        CONCATENATE 'BUSINESS RULES ID FOR CASHFLOW SUBROUTINE 3 NOT FOUND - MAINTAIN TVARVC VARIBALE "'
        TVARVC_NAME '".' INTO L_LOG RESPECTING BLANKS.
        CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).

     ls_message-msgid = 'BPC' .
     ls_message-msgty = 'E'.
     ls_message-msgno = '010' .
     ls_message-message = L_LOG.
     APPEND ls_message TO et_message.

       EXIT.
ENDIF.
*********** GET RULES **********************************************
        SELECT APPSET_ID
          APPLICATION_ID
          CALC_ID
          SEQ
          ACCOUNT
          DEST_ACCOUNT
          SUBTABLES
          DEST_SUBTABLES
          INVERT_SIGN
          FROM UJP_CALC_ACCOUNT INTO CORRESPONDING FIELDS OF TABLE LT_ACCOUNT_TR_RULES
          WHERE CALC_ID = LS_TVARVC-LOW
          AND APPSET_ID = I_APPSET_ID AND APPLICATION_ID = I_APPL_ID .
********************************************************************

CLEAR: LT_ACCOUNT_TR_RULES_EXPAND.

LOOP AT LT_ACCOUNT_TR_RULES INTO LS_ACCOUNT_TR_RULES.

  CLEAR: ACCOUNTS, FLOWS, WA_MEMBER, LS_ACCOUNT_TR_RULES_EXPAND.

IF EXPAND = 'Y' OR EXPAND = ''.
  CALL METHOD ZCL_BPC_TOOLS_CLASSIC=>READ_MASTER_DATA_HIERARCHY
  EXPORTING
    I_APPSET_ID  = I_APPSET_ID
    I_DIMENSION  = 'ACCOUNT'
    I_PARENT_MBR = LS_ACCOUNT_TR_RULES-ACCOUNT
  IMPORTING
   OUTPUT_DATA  = ACCOUNTS.
ENDIF.

IF ACCOUNTS IS NOT INITIAL.

  LOOP AT ACCOUNTS INTO WA_MEMBER.

    LS_ACCOUNT_TR_RULES_EXPAND =  LS_ACCOUNT_TR_RULES.
    LS_ACCOUNT_TR_RULES_EXPAND-ACCOUNT = WA_MEMBER.

INSERT LS_ACCOUNT_TR_RULES_EXPAND INTO TABLE LT_ACCOUNT_TR_RULES_EXPAND.

ENDLOOP.

DELETE LT_ACCOUNT_TR_RULES WHERE ACCOUNT = LS_ACCOUNT_TR_RULES-ACCOUNT.

ENDIF.

ENDLOOP.

INSERT LINES OF LT_ACCOUNT_TR_RULES_EXPAND INTO TABLE LT_ACCOUNT_TR_RULES.


***********************************************************************************************
CLEAR: LT_ACCOUNT_TR_RULES_EXPAND, LS_ACCOUNT_TR_RULES, COUNTER.

LOOP AT LT_ACCOUNT_TR_RULES INTO LS_ACCOUNT_TR_RULES.

  CLEAR: ACCOUNTS, FLOWS, WA_MEMBER, LS_ACCOUNT_TR_RULES_EXPAND.

IF EXPAND = 'Y' OR EXPAND = ''.
CALL METHOD ZCL_BPC_TOOLS_CLASSIC=>READ_MASTER_DATA_HIERARCHY
  EXPORTING
    I_APPSET_ID  = I_APPSET_ID
    I_DIMENSION  = 'FLOW'
    I_PARENT_MBR = LS_ACCOUNT_TR_RULES-SUBTABLES
  IMPORTING
   OUTPUT_DATA  = FLOWS.
ENDIF.

IF FLOWS IS NOT INITIAL.

  LOOP AT FLOWS INTO WA_MEMBER.

     LS_ACCOUNT_TR_RULES_EXPAND =  LS_ACCOUNT_TR_RULES.
    LS_ACCOUNT_TR_RULES_EXPAND-SUBTABLES = WA_MEMBER.


INSERT LS_ACCOUNT_TR_RULES_EXPAND INTO TABLE LT_ACCOUNT_TR_RULES_EXPAND.

ENDLOOP.

DELETE LT_ACCOUNT_TR_RULES WHERE ACCOUNT = LS_ACCOUNT_TR_RULES-ACCOUNT AND SUBTABLES = LS_ACCOUNT_TR_RULES-SUBTABLES.

ENDIF.

ENDLOOP.

INSERT LINES OF LT_ACCOUNT_TR_RULES_EXPAND INTO TABLE LT_ACCOUNT_TR_RULES.

SORT LT_ACCOUNT_TR_RULES BY ACCOUNT
          DEST_ACCOUNT
          SUBTABLES
          DEST_SUBTABLES.
DELETE ADJACENT DUPLICATES FROM LT_ACCOUNT_TR_RULES.

OUTPUT_DATA = LT_ACCOUNT_TR_RULES.


**********************************************************************************************************************
************************************ READ_ACC_TRANS_RULES END   **********************************
  ENDMETHOD.


  METHOD READ_MASTER_DATA.
************************************ READ_MASTER_DATA START****************************************

    DATA: LO_DIM       TYPE REF TO CL_UJA_DIM,
          LR_DIM_DATA  TYPE REF TO IF_UJA_DIM_DATA,
          LT_ATTR_NAME TYPE UJA_T_ATTR_NAME,
*      lt_sel TYPE uj0_t_sel,
          LS_SEL       TYPE UJ0_S_SEL,
          LR_DATA      TYPE REF TO DATA,
          LS_EMP       TYPE REF TO DATA.

    FIELD-SYMBOLS: <LT_DIMESNION_DATA> TYPE STANDARD TABLE.
*               <ls_dimension_data> TYPE ANY.

    REFRESH: LT_ATTR_NAME.

    TRY .

        CREATE OBJECT LO_DIM
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            I_DIMENSION = I_DIMENSION_ID.

      CATCH CX_UJA_ADMIN_ERROR .
    ENDTRY.

    LR_DIM_DATA = LO_DIM.

    " Append the list of attribute(s) for which the master data is generated
*APPEND: 'ID' TO lt_attr_name.

    " Bind the condition data to lt_sel table, this will become selection criteria
    " analogous to the WHERE clause of a DB SELECT statement

* IN THIS EXAMPLE LT_SEL IS CHANGING PARAM, WHICH MIGHT NOT BE THE CASE IN THE FINAL
* IMPLEMENTATION

    IF LT_SEL IS INITIAL.

      IF PROPERTY_ID IS INITIAL AND PROPERTY_VALUE IS INITIAL.

      LS_SEL-DIMENSION = I_DIMENSION_ID.
      LS_SEL-ATTRIBUTE = 'CALC'.
      LS_SEL-SIGN = 'I'.
      LS_SEL-OPTION = 'EQ'.
      LS_SEL-LOW = 'N'.
      APPEND LS_SEL TO LT_SEL.

      ELSE.

      LS_SEL-DIMENSION = I_DIMENSION_ID.
      LS_SEL-ATTRIBUTE = PROPERTY_ID.
      LS_SEL-SIGN = 'I'.
      LS_SEL-OPTION = 'EQ'.
      LS_SEL-LOW = PROPERTY_VALUE.
      APPEND LS_SEL TO LT_SEL.

      ENDIF.

    ENDIF.

    " GET DIMENSION MEMBERS
    TRY.
        CALL METHOD LR_DIM_DATA->READ_MBR_DATA
          EXPORTING
*            IT_ATTR_LIST = LT_ATTR_NAME    "attribute list
            IT_SEL       = LT_SEL          "condition data
          IMPORTING
            ER_DATA      = LR_DATA.        "reference of master data table

      CATCH CX_UJA_ADMIN_ERROR .
    ENDTRY.

    "Assign the referenced memory area to a field-symbol
* ASSIGN lr_data->* TO <lt_dimesnion_data>.

    OUTPUT_R_DATA = LR_DATA.

*CREATE DATA LS_EMP LIKE LINE OF <LT_DIMESNION_DATA>.
*ASSIGN LS_EMP->* TO <LS_DIMENSION_DATA>.


  ENDMETHOD.


  METHOD READ_MASTER_DATA_HIERARCHY.

    DATA:    LO_DIM      TYPE REF TO CL_UJA_DIM,
             LR_DATA     TYPE REF TO DATA,
             LR_DIM_DATA TYPE REF TO IF_UJA_DIM_DATA,
             LT_BASE_EN  TYPE UJA_T_DIM_MEMBER.
    TRY.
        CREATE OBJECT LO_DIM
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            I_DIMENSION = I_DIMENSION.

      CATCH CX_UJA_ADMIN_ERROR.
    ENDTRY.

    LR_DIM_DATA = LO_DIM.

    "GET THE CHILD NODES
    CALL METHOD LR_DIM_DATA->GET_CHILDREN_MBR
      EXPORTING
        I_PARENT_MBR     = I_PARENT_MBR
        IF_ONLY_BASE_MBR = 'X'
      IMPORTING
        ET_MEMBER        = LT_BASE_EN.

    OUTPUT_DATA = LT_BASE_EN.


************************************ READ_MASTER_DATA END *****************************************

  ENDMETHOD.


  METHOD READ_MODEL_DATA.
************************************ READ_MODEL_DATA START ****************************************

** 1. **--------- Data Declarations -------**
    DATA: LT_SEL         TYPE UJ0_T_SEL, "Selection criteria table
          LS_SEL         TYPE UJ0_S_SEL,
          LS_CV          TYPE UJK_S_CV,      " Logic Current View
          I_APPL_ID_INITIAL TYPE UJ_APPSET_ID,
          LT_DIM_MEMBER  TYPE UJA_T_DIM_MEMBER,
          LS_DIM_MEMBER  LIKE LINE OF LT_DIM_MEMBER,
          LO_APPL        TYPE REF TO CL_UJA_APPLICATION,
          LT_APPL_DIM    TYPE UJA_T_APPL_DIM,
          LS_APPL_DIM    LIKE LINE OF LT_APPL_DIM,
          LT_DIM_NAME    TYPE UJQ_T_DIM,
          LS_DIM_NAME    LIKE LINE OF LT_DIM_NAME,
          LO_MODEL       TYPE REF TO IF_UJ_MODEL,
          LO_DATAREF     TYPE REF TO DATA,
          LO_QUERY       TYPE REF TO IF_UJO_QUERY,
          LV_END_OF_DATA TYPE RS_BOOL,
          LT_MESSAGE     TYPE UJ0_T_MESSAGE.

    FIELD-SYMBOLS:  <LT_QUERY_RESULT> TYPE STANDARD TABLE.

**---------------End of Data Declaration----------------------**


*---- 2. Create an object  for  the input parameters such i_appset_id,  i_appl_id.-------*

"Constructor specifies which model to read - self by defaul
"If READ_MODEL is passed - it means we want to read different model
"Then read different model, which is in READ_MODEL

I_APPL_ID_INITIAL = I_APPL_ID.


IF READ_MODEL IS NOT INITIAL.
  I_APPL_ID = READ_MODEL.
ENDIF.

"Specific clause for special people using case sensitive model names in BPC
"We will try to read Customer FPRS to get us some cashflow
IF I_APPSET_ID = 'Customer_FPRS' AND I_APPL_ID = 'CONSOLIDATION'.
  I_APPL_ID = 'Consolidation'.
ENDIF.

    CREATE OBJECT LO_APPL
      EXPORTING
        I_APPSET_ID      = I_APPSET_ID
        I_APPLICATION_ID = I_APPL_ID.

*---- 3. Use this object to read the dimension for the  i_appl_id  & Append ' Measures ' to the dimension table -----*

    REFRESH LT_APPL_DIM.
    LO_APPL->GET_APPL_DIM(
    EXPORTING
    I_APPL_ID   = I_APPL_ID
    IMPORTING
    ET_APPL_DIM = LT_APPL_DIM ). "dimension table
    REFRESH LT_DIM_NAME.

**Populate dimension table 'lt_dim_name'.

    LOOP AT LT_APPL_DIM INTO LS_APPL_DIM.
      LS_DIM_NAME = LS_APPL_DIM-DIMENSION.
      APPEND LS_DIM_NAME TO LT_DIM_NAME.
      CLEAR LS_DIM_NAME.
    ENDLOOP.

* Include ' Measures ' as dimension table *
*  ls_dim_name  = 'MEASURES'.
*  APPEND ls_dim_name TO lt_dim_name.
*  SORT lt_dim_name.

*--4. Prepare Selection range table say for ex :  'lt_sel '  *.
* if it_sel[] is initial.
    LOOP AT  LT_DIM_NAME INTO LS_DIM_NAME  .
      CLEAR : LS_CV .

      READ TABLE IT_SEL INTO LS_SEL WITH KEY DIMENSION = LS_DIM_NAME .
      IF SY-SUBRC = 0.
        LOOP AT IT_SEL INTO LS_SEL WHERE DIMENSION = LS_DIM_NAME .
          APPEND LS_SEL TO LT_SEL.
        ENDLOOP.
        CONTINUE.
      ENDIF.
* Read from scope for each dimension from current view table*
      IF IT_CV IS NOT INITIAL.

        READ TABLE IT_CV INTO LS_CV WITH KEY DIMENSION =  LS_DIM_NAME .
        IF SY-SUBRC = 0 . "and ls_cv-USER_SPECIFIED = abap_true.
          LOOP AT LS_CV-MEMBER INTO LS_DIM_MEMBER.
            LS_SEL-DIMENSION = LS_CV-DIMENSION.
            LS_SEL-ATTRIBUTE = 'ID'.
            LS_SEL-SIGN = 'I'.
            LS_SEL-OPTION = 'EQ'.
            LS_SEL-LOW = LS_DIM_MEMBER.
            APPEND LS_SEL TO LT_SEL.
            CLEAR LS_DIM_MEMBER.
          ENDLOOP.
          CLEAR LT_DIM_MEMBER.
        ENDIF.
      ENDIF.
    ENDLOOP.


* else.
*   lt_sel[] = it_sel[].
* endif.

*---5. Create a reference structure similar to ct_data using the method -----*

    TRY.
        LO_MODEL = CL_UJ_MODEL=>GET_MODEL( I_APPSET_ID ).
        LO_MODEL->CREATE_TX_DATA_REF(
        EXPORTING
        I_APPL_NAME  = I_APPL_ID
        I_TYPE       = 'T'
        IT_DIM_NAME  = LT_DIM_NAME
        IF_TECH_NAME = SPACE
        IMPORTING
        ER_DATA      = LO_DATAREF ).
      CATCH CX_UJ_STATIC_CHECK.
    ENDTRY.
* Assigning the structure to table
    ASSIGN LO_DATAREF->* TO <LT_QUERY_RESULT>.

**Run  a query using method  '  run_rsdri_query ' **
    TRY.

        LO_QUERY = CL_UJO_QUERY_FACTORY=>GET_QUERY_ADAPTER(
        I_APPSET_ID = I_APPSET_ID
        I_APPL_ID   = I_APPL_ID
        ).
** Run Query to populate ct_data based on dimensions , selection criteria **.

        WHILE LV_END_OF_DATA = RS_C_FALSE.

          LO_QUERY->RUN_RSDRI_QUERY(

          EXPORTING
          IT_DIM_NAME       =  LT_DIM_NAME " BPC: Dimension List
          IT_RANGE          =  LT_SEL     " BPC: Selection condition
          IF_CHECK_SECURITY = ABAP_FALSE   " BPC: Generic indicator

          IMPORTING
          ET_DATA           = <LT_QUERY_RESULT>
          E_END_OF_DATA     = LV_END_OF_DATA    " BPC: Last Data Package Yes/No
          ET_MESSAGE        = LT_MESSAGE    " BPC: Messages
          ).

*        LOOP AT <lt_query_result> ASSIGNING <ls_query_result>.
*        APPEND <ls_query_result> TO output_data.
*        ENDLOOP.

        ENDWHILE.
      CATCH CX_UJO_READ.  " Exception of common read

    ENDTRY.

*-- 6.  Copy data into output_data ----*

 I_APPL_ID = I_APPL_ID_INITIAL.

    OUTPUT_DATA = <LT_QUERY_RESULT>.

************************************ READ_MODEL_DATA END ******************************************
  ENDMETHOD.


  METHOD READ_MODEL_DATA_AXIS.

    DATA:
      LT_APPL_DIM TYPE UJA_T_APPL_DIM,
      LS_APPL_DIM LIKE LINE OF LT_APPL_DIM,
      LS_SEL      LIKE LINE OF IT_SEL,
      LT_DIM_NAME TYPE UJQ_T_DIM,
      LS_DIM_NAME LIKE LINE OF LT_DIM_NAME,
      "LO_MODEL       TYPE REF TO IF_UJ_MODEL,
      "LO_DATAREF     TYPE REF TO DATA,
      "LO_QUERY       TYPE REF TO IF_UJO_QUERY,
      "LV_END_OF_DATA TYPE RS_BOOL,
      "LT_DIMENSIONS  TYPE UJA_S_APPSET,
      LO_APPL     TYPE REF TO CL_UJA_APPLICATION,
      LO_APPL_MGR TYPE REF TO IF_UJA_APPLICATION_MANAGER,
      CV_MEMBER   TYPE UJK_S_CV,
      WA_MEMBER   TYPE UJ_DIM_MEMBER.



      DATA:
        LT_DIM_LIST TYPE UJA_T_DIM_LIST,
      LT_AXIS     TYPE UJO_T_QUERY_DIM,
      LS_AXIS     TYPE UJO_T_MEMBERS,
      LS_MEMBER   TYPE UJO_S_MEMBER,
      LT_SLICER   TYPE UJO_T_MEMBERS,
      LO_SQE      TYPE REF TO IF_UJO_QUERY,
      LR_DATA     TYPE REF TO DATA.


    FIELD-SYMBOLS: <LT_QUERY_RESULT> TYPE STANDARD TABLE.



*************************Derive Dimension Lists
**---- 2. Create an object  for  the input parameters such i_appset_id,  i_appl_id.-------*
*
    CREATE OBJECT LO_APPL
      EXPORTING
        I_APPSET_ID      = I_APPSET_ID
        I_APPLICATION_ID = I_APPL_ID.
*
**---- 3. Use this object to read the dimension for the  i_appl_id  & Append ' Measures ' to the dimension table -----*
*
    REFRESH LT_APPL_DIM.
    LO_APPL->GET_APPL_DIM(
    EXPORTING
    I_APPL_ID   = I_APPL_ID
    IMPORTING
    ET_APPL_DIM = LT_APPL_DIM ). "dimension table
    REFRESH LT_DIM_NAME.
*

**********Populate dimension table 'lt_dim_name'.

    LOOP AT LT_APPL_DIM INTO LS_APPL_DIM.
      LS_DIM_NAME = LS_APPL_DIM-DIMENSION.
      APPEND LS_DIM_NAME TO LT_DIM_NAME.
      CLEAR LS_DIM_NAME.
    ENDLOOP.


    LOOP AT LT_APPL_DIM INTO LS_APPL_DIM.

      LOOP AT IT_SEL INTO LS_SEL WHERE DIMENSION = LS_APPL_DIM-DIMENSION.

        LS_MEMBER-DIMENSION = LS_SEL-DIMENSION.
        LS_MEMBER-MEMBER = LS_SEL-LOW.
        APPEND LS_MEMBER TO  LS_AXIS.

      ENDLOOP.
      IF SY-SUBRC = 0.
        APPEND LS_AXIS TO  LT_AXIS.
      ELSE.

        "" For dimensions not definded in LT_SEL
        CLEAR CV_MEMBER.

        READ TABLE IT_CV INTO CV_MEMBER WITH TABLE KEY DIM_UPPER_CASE = LS_APPL_DIM-DIMENSION.
        LS_MEMBER-DIMENSION = LS_APPL_DIM-DIMENSION.

        LOOP AT CV_MEMBER-MEMBER INTO WA_MEMBER.
          LS_MEMBER-MEMBER =  WA_MEMBER.
          APPEND LS_MEMBER TO  LS_AXIS.
        ENDLOOP.

        APPEND LS_AXIS TO  LT_AXIS.
      ENDIF.

      CLEAR LS_AXIS.
    ENDLOOP.



*--5. Create a reference structure similar to ct_data using the method -----*
*************************************************************************
* Create data ref
*************************************************************************
    CALL METHOD CL_UJA_BPC_ADMIN_FACTORY=>GET_APPLICATION_MANAGER
      EXPORTING
        I_APPSET_ID      = I_APPSET_ID
        I_APPLICATION_ID = I_APPL_ID
      RECEIVING
        RO_RETURN        = LO_APPL_MGR.

    CALL METHOD LO_APPL_MGR->CREATE_DATA_REF
      EXPORTING
        I_DATA_TYPE   = 'T'
        IT_DIM_NAME   = LT_DIM_LIST
        IF_TECH_NAME  = ABAP_FALSE
        IF_SIGNEDDATA = ABAP_TRUE
      IMPORTING
        ER_DATA       = LR_DATA.

    ASSIGN LR_DATA->* TO <LT_QUERY_RESULT>.
*************************************************************************
* Query
*************************************************************************
    CALL METHOD CL_UJO_QUERY_FACTORY=>GET_QUERY_ADAPTER
      EXPORTING
        I_APPSET_ID = I_APPSET_ID
        I_APPL_ID   = I_APPL_ID
      RECEIVING
        ADAPTER     = LO_SQE.
    CALL METHOD LO_SQE->RUN_AXIS_QUERY_SYMM
      EXPORTING
        IT_AXIS   = LT_AXIS
        IT_SLICER = LT_SLICER
      IMPORTING
        ET_DATA   = <LT_QUERY_RESULT>.

    OUTPUT_DATA = <LT_QUERY_RESULT>.

  ENDMETHOD.


  METHOD TARGET_ACCS.

******* EXPAND ACCOUNTS WITH SOURCES
"This method is used to expand the scope of model read with the target accounts for carry-forward
"It recieves the list of accounts, queries business rules and then populates the target accounts
"So that the data could be read from the model with the correct intersection

  DATA: TVARVC_NAME         TYPE RVARI_VNAM.
        DATA: LT_ACCOUNT_TR_RULES TYPE HASHED TABLE OF UJP_CALC_ACCOUNT WITH UNIQUE KEY APPSET_ID APPLICATION_ID CALC_ID
          SEQ
          ACCOUNT
          DEST_ACCOUNT
          SUBTABLES
          DEST_SUBTABLES
          INVERT_SIGN,
         LT_ACCOUNT_COMPOUNDED TYPE HASHED TABLE OF UJP_CALC_ACCOUNT WITH UNIQUE KEY CALC_ID
          SEQ
          ACCOUNT
          DEST_ACCOUNT
          SUBTABLES
          DEST_SUBTABLES
          INVERT_SIGN,

              LS_ACCOUNT_TR_RULES LIKE LINE OF LT_ACCOUNT_TR_RULES,
           WA_MEMBER        TYPE UJ_DIM_MEMBER,
           CV_ACCOUNTS      TYPE UJK_S_CV.



        CONCATENATE I_APPSET_ID '-' I_APPL_ID '-' 'SUB3' INTO TVARVC_NAME.
        CLEAR ET_MESSAGE.


        CALL METHOD READ_ACC_TRANS_RULESV2
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            I_APPL_ID   = I_APPL_ID
            TVARVC_NAME = TVARVC_NAME
          IMPORTING
            ET_MESSAGE  = ET_MESSAGE
            OUTPUT_DATA = LT_ACCOUNT_TR_RULES.



        IF ET_MESSAGE IS NOT INITIAL.
          RAISE EXCEPTION TYPE CX_UJ_CUSTOM_LOGIC .
        ENDIF.

        INSERT LINES OF LT_ACCOUNT_TR_RULES INTO TABLE LT_ACCOUNT_COMPOUNDED.
        CLEAR LT_ACCOUNT_TR_RULES.
*
                CONCATENATE I_APPSET_ID '-' I_APPL_ID '-' 'SUB5' INTO TVARVC_NAME.
        CLEAR ET_MESSAGE.


        CALL METHOD READ_ACC_TRANS_RULESV2
          EXPORTING
            I_APPSET_ID = I_APPSET_ID
            I_APPL_ID   = I_APPL_ID
            EXPAND = 'N'
            TVARVC_NAME = TVARVC_NAME
          IMPORTING
            ET_MESSAGE  = ET_MESSAGE
            OUTPUT_DATA = LT_ACCOUNT_TR_RULES.



        IF ET_MESSAGE IS NOT INITIAL.
          RAISE EXCEPTION TYPE CX_UJ_CUSTOM_LOGIC .
        ENDIF.

                INSERT LINES OF LT_ACCOUNT_TR_RULES INTO TABLE LT_ACCOUNT_COMPOUNDED.
        CLEAR LT_ACCOUNT_TR_RULES.


 READ TABLE IT_CV INTO CV_ACCOUNTS WITH TABLE KEY DIM_UPPER_CASE = 'ACCOUNT'.
        LOOP AT CV_ACCOUNTS-MEMBER INTO WA_MEMBER.
           READ TABLE LT_ACCOUNT_COMPOUNDED WITH KEY ACCOUNT = WA_MEMBER TRANSPORTING NO FIELDS.
           IF SY-SUBRC <> 0.
             DELETE LT_ACCOUNT_COMPOUNDED WHERE ACCOUNT = WA_MEMBER.
           ELSE.
            EXIT.
           ENDIF.
        ENDLOOP.

TARGET_ACCOUNTS = LT_ACCOUNT_COMPOUNDED.





  ENDMETHOD.


  METHOD TIME.
"Set time scope depending on the category in IT_CV
***** Time structure definition ********************
    DATA: BEGIN OF LS_TIME,
            ID        TYPE CHAR32,
            CURRMONTH TYPE CHAR32,
            BUDMONTH  TYPE CHAR32,
            TIMEID    TYPE CHAR32,
            VAT_QLY   TYPE CHAR32,
          END OF LS_TIME.
****************************************************

DATA:                LT_TIMESCOPE      LIKE SORTED TABLE OF LS_TIME WITH UNIQUE KEY ID,
                     LT_TIMEQRTLY      LIKE SORTED TABLE OF LS_TIME WITH UNIQUE KEY ID,
                     LS_TIMEQRTLY      LIKE LS_TIME,
                     COUNTER           TYPE UJ_SMALLINT,
                     MASTER_DATA       TYPE REF TO DATA.

FIELD-SYMBOLS: <DIMENSION_DATA> TYPE STANDARD TABLE.

***** GET TIME VARIABLES *************************************
    me->READ_MASTER_DATA(
      EXPORTING
        I_DIMENSION_ID   = 'TIME'
      IMPORTING
        OUTPUT_R_DATA = MASTER_DATA ).

    ASSIGN MASTER_DATA->* TO <DIMENSION_DATA>.
    MOVE-CORRESPONDING <DIMENSION_DATA> TO LT_TIMESCOPE.

    "Assign TimeScope
    DELETE LT_TIMESCOPE WHERE ID+5(2) = 'IN' OR CURRMONTH = 'A'.
    DELETE LT_TIMESCOPE WHERE CURRMONTH = '' AND BUDMONTH = ''.

    READ TABLE LT_TIMESCOPE INDEX 1 INTO LS_TIME.
    CURRENT_PERIOD = LS_TIME-ID. "ALWAYS FIRST PERIOD


******************** END COMMON VARIABLES ********************

    LOOP AT LT_TIMESCOPE INTO LS_TIME.

      IF CATEGORY = 'WFORECAST'. "Forecast Category

        IF LS_TIME-BUDMONTH = 'B' AND LS_TIME-CURRMONTH <> 'F'.
          DELETE LT_TIMESCOPE INDEX SY-TABIX.
        ENDIF.

      ELSEIF CATEGORY = 'BUDGET'. "Budget Category

        IF LS_TIME-BUDMONTH <> 'B'.
          DELETE LT_TIMESCOPE INDEX SY-TABIX.
        ENDIF.

        ELSE. "No category or any other

      ENDIF.
    ENDLOOP.

    "Assign Time Quarterly
    LT_TIMEQRTLY = LT_TIMESCOPE.
    DELETE LT_TIMEQRTLY WHERE VAT_QLY NE 'Y'.

*GET LAST AND FIRST PERIODS IN TIMEID FORMAT START
    DESCRIBE TABLE LT_TIMESCOPE LINES COUNTER.
    READ TABLE LT_TIMESCOPE INDEX COUNTER INTO LS_TIME.
    LAST_PERIOD = LS_TIME-TIMEID.
    READ TABLE LT_TIMESCOPE INDEX 1 INTO LS_TIME.
    FIRST_PERIOD = LS_TIME-TIMEID.
*GET LAST PERIOD IN TIMEID FORMAT END

"Change on 21.03.2017 regarding Actualisation scope extend
   "GET BACK ONE PERIOD FOR ACTUALISATION
 IF EXTEND = 'A'.

    CLEAR LS_TIME.
      me->PERIOD_OFFSET(
        EXPORTING
          IMPORT_PERIOD = CURRENT_PERIOD
          OFFSET        = -1
        RECEIVING
          EXPORT_PERIOD = LS_TIME-ID ).

      LS_TIME-CURRMONTH = 'C'.
      LS_TIME-TIMEID = LS_TIME-ID(4) && LS_TIME-ID+5(2) && '00'.
      INSERT LS_TIME INTO TABLE LT_TIMESCOPE.

      CURRENT_PERIOD = LS_TIME-ID.

*GET LAST AND FIRST PERIODS IN TIMEID FORMAT START
    DESCRIBE TABLE LT_TIMESCOPE LINES COUNTER.
    READ TABLE LT_TIMESCOPE INDEX COUNTER INTO LS_TIME.
    LAST_PERIOD = LS_TIME-TIMEID.
    READ TABLE LT_TIMESCOPE INDEX 1 INTO LS_TIME.
    FIRST_PERIOD = LS_TIME-TIMEID.
*GET LAST PERIOD IN TIMEID FORMAT END


 ENDIF.
"

TIMESCOPE = LT_TIMESCOPE.
TIMEQRTLY = LT_TIMEQRTLY.

DATA:  TARGET_PERIOD     TYPE CHAR8.

DATA: SOURCE_MONTH TYPE N LENGTH 2,
      SOURCE_YEAR TYPE char4.

SOURCE_YEAR = LAST_PERIOD(4).
SOURCE_MONTH = LAST_PERIOD+4(2).


IF EXTEND IS NOT INITIAL AND EXTEND <> 'A'.
  CLEAR LT_TIMESCOPE.
  MOVE-CORRESPONDING <DIMENSION_DATA> TO LT_TIMESCOPE.
  DELETE LT_TIMESCOPE WHERE ID+5(2) = 'IN'.

  DO EXTEND TIMES.
  SOURCE_MONTH = SOURCE_MONTH + 1.
  IF SOURCE_MONTH = 13.
    SOURCE_YEAR = SOURCE_YEAR + 1.
    SOURCE_MONTH = 01.
  ENDIF.
  TARGET_PERIOD = SOURCE_YEAR && SOURCE_MONTH && '00'.
  ENDDO.
    LOOP AT LT_TIMESCOPE INTO LS_TIME.
      IF LS_TIME-TIMEID BETWEEN FIRST_PERIOD AND TARGET_PERIOD.
        ELSE.
          DELETE LT_TIMESCOPE INDEX SY-TABIX.
      ENDIF.
    ENDLOOP.

    "Assign Time Quarterly
    LT_TIMEQRTLY = LT_TIMESCOPE.
    DELETE LT_TIMEQRTLY WHERE VAT_QLY NE 'Y'.

*GET LAST AND FIRST PERIODS IN TIMEID FORMAT START
    DESCRIBE TABLE LT_TIMESCOPE LINES COUNTER.
    READ TABLE LT_TIMESCOPE INDEX COUNTER INTO LS_TIME.
    LAST_PERIOD = LS_TIME-TIMEID.
    READ TABLE LT_TIMESCOPE INDEX 1 INTO LS_TIME.
    FIRST_PERIOD = LS_TIME-TIMEID.
*GET LAST PERIOD IN TIMEID FORMAT END

TIMESCOPE = LT_TIMESCOPE.
TIMEQRTLY = LT_TIMEQRTLY.

  ENDIF.



  ENDMETHOD.


  METHOD WRITE_MODEL_DATA.
************************************ WRITE_MODEL_DATA START ***************************************

    DATA:   MODEL TYPE UJ_APPL_ID,
            LS_WB_PARAM    TYPE IF_UJO_WRITE_BACK=>GS_WB_PARAM,
            LS_PARAM         TYPE UJK_S_SCRIPT_LOGIC_HASHENTRY.

    FIELD-SYMBOLS: <COMMIT_DATA> TYPE STANDARD TABLE.

*********** Get parameters from script logic *******************************
*** Pass the required parameters via script logic param and adjust this code
  CLEAR: LS_PARAM, ET_MESSAGE.
    READ TABLE IT_PARAM WITH KEY HASHKEY = 'RUN_DEFAULT' INTO LS_PARAM.
***************************************************************************

    IF TARGET_MODEL IS INITIAL.

      ASSIGN INPUT_DATA TO <COMMIT_DATA>.
      MODEL = I_APPL_ID.

    ELSE.

      DATA:  LO_APPL_MGR    TYPE REF TO IF_UJA_APPLICATION_MANAGER,
             LS_APPLICATION TYPE UJA_S_APPLICATION,
             LS_DIMENSIONS  TYPE UJA_S_DIMENSION.

      MODEL = TARGET_MODEL.

      LO_APPL_MGR = CL_UJA_BPC_ADMIN_FACTORY=>GET_APPLICATION_MANAGER(
       I_APPSET_ID = I_APPSET_ID
       I_APPLICATION_ID = MODEL ).
      CLEAR LS_APPLICATION.
      LO_APPL_MGR->GET(
       EXPORTING
       IF_WITH_MEASURES = ABAP_FALSE " BPC: Generic indicator
       IF_SUMMARY = ABAP_FALSE " BPC: Generic indicator
       IMPORTING
       ES_APPLICATION = LS_APPLICATION ). " Applications table type

      REFRESH LT_DIM_LIST.
      LOOP AT LS_APPLICATION-DIMENSIONS INTO LS_DIMENSIONS.
        APPEND LS_DIMENSIONS-DIMENSION TO LT_DIM_LIST.
      ENDLOOP.
      LO_APPL_MGR->CREATE_DATA_REF(
       EXPORTING
       I_DATA_TYPE = 'T'
       IT_DIM_NAME = LT_DIM_LIST
       IF_TECH_NAME = ABAP_FALSE
       IF_SIGNEDDATA = ABAP_TRUE
       IMPORTING
       ER_DATA = LR_DATA ).

      ASSIGN LR_DATA->* TO <COMMIT_DATA>.

    ENDIF.

IF WRITE = 'Y'.

  IF <COMMIT_DATA> IS INITIAL.
  <COMMIT_DATA> = INPUT_DATA.
  ENDIF.

*************************************************
***** WRITE BACK ********************************
    DATA: LO_UJO_WB      TYPE REF TO IF_UJO_WRITE_BACK,
          LS_WB_STATUS   TYPE UJO_S_WB_STATUS,
          LS_WORK_STATUS TYPE UJR_S_WORK_STATUS,
          LS_AUDIT       TYPE UJR_S_UPDATE_AUDIT,
          LV_MEASURE     TYPE UJ_DIM_MEMBER VALUE 'PERIODIC'.

    LS_WORK_STATUS-MODULE_ID = UJ00_C_MOD_NAME_DM.
    LS_WORK_STATUS-BLOCKSTATUS = 0.
    LS_AUDIT-ACTCODE = UJU0_CS_ACT_CODE-LOGIC_EXE.
    LO_UJO_WB = CL_UJO_WB_FACTORY=>CREATE_WRITE_BACK( ).
    LS_WB_PARAM = CL_UJO_WB_FACTORY=>DEFAULT_WB_PARAM( ).
    LS_WB_PARAM-WORK_STATUS = LS_WORK_STATUS.
     IF LS_PARAM-HASHVALUE = 'Y'.
     LS_WB_PARAM-DEFAULT_LOGIC = ABAP_TRUE.
     ENDIF.
    LS_WB_PARAM-UPDATE_AUDIT = ABAP_TRUE.
    LS_WB_PARAM-DUPLICATE = ABAP_TRUE.
    LS_WB_PARAM-CALC_DELTA = ABAP_TRUE.
    LS_WB_PARAM-MDATA_CHECK = ABAP_FALSE.
    LS_WB_PARAM-SIGN_TRANS = ABAP_TRUE.
    LS_WB_PARAM-MEASURES_FORMULA = LV_MEASURE.
    LS_WB_PARAM-AUDIT_INFO = LS_AUDIT.

    LO_UJO_WB->WRITE_BACK(
    EXPORTING
     I_APPSET_ID = I_APPSET_ID
     I_APPL_ID = MODEL
     IS_WB_PARAM = LS_WB_PARAM
     IT_RECORDS = <COMMIT_DATA>
    IMPORTING
     ES_WB_STATUS = LS_WB_STATUS
     "ET_ERROR_RECORDS = ET_ERROR_RECORDS
     ET_MESSAGE = ET_MESSAGE ).

ENDIF.

************************************ WRITE_MODEL_DATA END ****************************************
  ENDMETHOD.
ENDCLASS.
