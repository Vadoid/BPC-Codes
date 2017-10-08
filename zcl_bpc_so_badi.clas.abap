class ZCL_BPC_SO_BADI definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_UJ_CUSTOM_LOGIC .

  methods CASHFLOW
    importing
      !I_APPSET_ID type UJ_APPSET_ID
      !I_APPL_ID type UJ_APPL_ID
      !IT_CV type UJK_T_CV
      !SUB_ID type CHAR05 optional
      !WORKING_CATEGORY type STRING
    exporting
      !ET_MESSAGE type UJ0_T_MESSAGE
    changing
      !CT_DATA type STANDARD TABLE optional
    raising
      CX_UJ_CUSTOM_LOGIC .
  methods INC_STATEMENT
    importing
      !I_APPSET_ID type UJ_APPSET_ID
      !I_APPL_ID type UJ_APPL_ID
      !IT_CV type UJK_T_CV
      !SUB_ID type CHAR05 optional
      !WORKING_CATEGORY type STRING
    exporting
      !ET_MESSAGE type UJ0_T_MESSAGE
    changing
      !CT_DATA type STANDARD TABLE optional
    raising
      CX_UJ_CUSTOM_LOGIC .
  methods CF_ACT_MAIN
    importing
      !I_APPSET_ID type UJ_APPSET_ID
      !I_APPL_ID type UJ_APPL_ID
      !IT_CV type UJK_T_CV
      !SUB_ID type CHAR05
      !WORKING_CATEGORY type STRING
    exporting
      !ET_MESSAGE type UJ0_T_MESSAGE
    changing
      !CT_DATA type STANDARD TABLE optional
    raising
      CX_UJ_CUSTOM_LOGIC .
  methods IS_ACT_MAIN
    importing
      !I_APPSET_ID type UJ_APPSET_ID
      !I_APPL_ID type UJ_APPL_ID
      !IT_CV type UJK_T_CV
      !SUB_ID type CHAR05
      !WORKING_CATEGORY type STRING
    exporting
      !ET_MESSAGE type UJ0_T_MESSAGE
    changing
      !CT_DATA type STANDARD TABLE optional
    raising
      CX_UJ_CUSTOM_LOGIC .
  methods CLEAR_INACTIVE
    importing
      !I_APPSET_ID type UJ_APPSET_ID
      !I_APPL_ID type UJ_APPL_ID
      !IT_CV type UJK_T_CV
      !IT_PARAM type UJK_T_SCRIPT_LOGIC_HASHTABLE optional
      !SUB_ID type CHAR05
      !WORKING_CATEGORY type STRING
    exporting
      !ET_MESSAGE type UJ0_T_MESSAGE
    changing
      !CT_DATA type STANDARD TABLE optional
    raising
      CX_UJ_CUSTOM_LOGIC .
  methods CLEAR
    importing
      !I_APPSET_ID type UJ_APPSET_ID
      !I_APPL_ID type UJ_APPL_ID
      !IT_CV type UJK_T_CV
      !SUB_ID type CHAR05
      !WORKING_CATEGORY type STRING
    exporting
      !ET_MESSAGE type UJ0_T_MESSAGE
    changing
      !CT_DATA type STANDARD TABLE optional
    raising
      CX_UJ_CUSTOM_LOGIC .
  methods CREATE_SNAPSHOT
    importing
      !I_APPSET_ID type UJ_APPSET_ID
      !I_APPL_ID type UJ_APPL_ID
      !IT_CV type UJK_T_CV
      !SUB_ID type CHAR05
      !WORKING_CATEGORY type STRING
    exporting
      !ET_MESSAGE type UJ0_T_MESSAGE
    changing
      !CT_DATA type STANDARD TABLE optional
    raising
      CX_UJ_CUSTOM_LOGIC .
  methods TRANSFER_IS_BS
    importing
      !I_APPSET_ID type UJ_APPSET_ID
      !I_APPL_ID type UJ_APPL_ID
      !IT_CV type UJK_T_CV
      !IT_PARAM type UJK_T_SCRIPT_LOGIC_HASHTABLE optional
      !WORKING_CATEGORY type STRING
    exporting
      !ET_MESSAGE type UJ0_T_MESSAGE
    changing
      !CT_DATA type STANDARD TABLE optional
    raising
      CX_UJ_CUSTOM_LOGIC .
  methods TRANSFER_CP_IS   "Transfer Capex to Income Statement
    importing
      !I_APPSET_ID type UJ_APPSET_ID
      !I_APPL_ID type UJ_APPL_ID
      !IT_CV type UJK_T_CV
      !IT_PARAM type UJK_T_SCRIPT_LOGIC_HASHTABLE optional
      !WORKING_CATEGORY type STRING
    exporting
      !ET_MESSAGE type UJ0_T_MESSAGE
    changing
      !CT_DATA type STANDARD TABLE optional
    raising
      CX_UJ_CUSTOM_LOGIC .
  methods TRANSFER_RS_IS
    importing
      !I_APPSET_ID type UJ_APPSET_ID
      !I_APPL_ID type UJ_APPL_ID
      !IT_CV type UJK_T_CV
      !IT_PARAM type UJK_T_SCRIPT_LOGIC_HASHTABLE optional
      !WORKING_CATEGORY type STRING
      !SUB_ID type CHAR05 optional
    exporting
      !ET_MESSAGE type UJ0_T_MESSAGE
    changing
      !CT_DATA type STANDARD TABLE optional
    raising
      CX_UJ_CUSTOM_LOGIC .
  methods FXTRANS
    importing
      !I_APPSET_ID type UJ_APPSET_ID
      !I_APPL_ID type UJ_APPL_ID
      !IT_PARAM type UJK_T_SCRIPT_LOGIC_HASHTABLE optional
      !IT_CV type UJK_T_CV
      !FLAG type CHAR1 optional
    exporting
      !ET_MESSAGE type UJ0_T_MESSAGE
    changing
      !CT_DATA type STANDARD TABLE optional
    raising
      CX_UJ_CUSTOM_LOGIC .
  methods CAPEX
    importing
      !I_APPSET_ID type UJ_APPSET_ID
      !I_APPL_ID type UJ_APPL_ID
      !IT_PARAM type UJK_T_SCRIPT_LOGIC_HASHTABLE optional
      !IT_CV type UJK_T_CV
    exporting
      !ET_MESSAGE type UJ0_T_MESSAGE
    changing
      !CT_DATA type STANDARD TABLE optional
    raising
      CX_UJ_CUSTOM_LOGIC .
  methods CAPEX_NEW
    importing
      !I_APPSET_ID type UJ_APPSET_ID
      !I_APPL_ID type UJ_APPL_ID
      !IT_PARAM type UJK_T_SCRIPT_LOGIC_HASHTABLE optional
      !IT_CV type UJK_T_CV
      !WORKING_CATEGORY type STRING
    exporting
      !ET_MESSAGE type UJ0_T_MESSAGE
    changing
      !CT_DATA type STANDARD TABLE optional
    raising
      CX_UJ_CUSTOM_LOGIC .
  methods RESOURCE_CALC
    importing
      !I_APPSET_ID type UJ_APPSET_ID
      !I_APPL_ID type UJ_APPL_ID
      !IT_PARAM type UJK_T_SCRIPT_LOGIC_HASHTABLE optional
      !IT_CV type UJK_T_CV
      !SUB_ID type CHAR05 optional
    exporting
      !ET_MESSAGE type UJ0_T_MESSAGE
    changing
      !CT_DATA type STANDARD TABLE optional
    raising
      CX_UJ_CUSTOM_LOGIC .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_BPC_SO_BADI IMPLEMENTATION.


  METHOD CAPEX.
*&---------------------------------------------------------------------*
****************************************************************************
* Program type....:  Customer SO Custom Method
* Author...........:   ()
* Date.............: January 2017
* ----------------------------------------------------------------------
* Description......: This method handles all the relevant calculations
*                    within the CAPEX model
*
*
****************************************************************************
* Change Log:
*     1. CMC004 - LC calcs were not preserved
*     2. CMC001 - Internal/External Split
*     3. SUB 2 CLEARING TO INCLUDE STATACCT
****************************************************************************
* Changed on: 03.04.2017             By: VZ
* Description:
****************************************************************************

    DATA: BEGIN OF LS_TIME,
            ID        TYPE CHAR32,
            CURRMONTH TYPE CHAR32,
            BUDMONTH  TYPE CHAR32,
            TIMEID    TYPE CHAR32,
            YEAR      TYPE CHAR32,
          END OF LS_TIME.

    DATA: BEGIN OF LS_CATEGORY,
            ID   TYPE CHAR32,
            YEAR TYPE CHAR32,
          END OF LS_CATEGORY.

    DATA: BEGIN OF LS_ASSET_CLASS,
            ID          TYPE CHAR32,
            BS_COSTACC  TYPE CHAR32,
            BS_DEPAMORT TYPE CHAR32,
            PL_GLACC    TYPE CHAR32,
            FLOW        TYPE CHAR32,
            FLOW_VAT    TYPE CHAR32,
            TRADE_CRED  TYPE CHAR32,
          END OF LS_ASSET_CLASS.

    DATA: BEGIN OF LS_CAPEX,
            ASSET       TYPE CHAR32,
            ASSET_CLASS TYPE CHAR32,
            AUDITTRAIL  TYPE CHAR32,
            CATEGORY    TYPE CHAR32,
            ENTITY      TYPE CHAR32,
            FLOW        TYPE CHAR32,
            RPTCURRENCY TYPE CHAR32,
            TIME        TYPE CHAR32,
            SIGNEDDATA  TYPE UJ_SDATA,
          END OF LS_CAPEX.

    DATA:    LT_TIMESCOPE      LIKE STANDARD TABLE OF LS_TIME, "SORTED TABLE OF LS_TIME WITH UNIQUE KEY ID,
             LT_TIMESCOPE_PLUS LIKE STANDARD TABLE OF LS_TIME, "LIKE SORTED TABLE OF LS_TIME WITH UNIQUE KEY ID,
             "LT_CAPEX_TX       LIKE STANDARD TABLE OF LS_CAPEX,
             LT_CAPEX_FLG      LIKE STANDARD TABLE OF LS_CAPEX,
             LT_CAPEX_XXX      LIKE STANDARD TABLE OF LS_CAPEX,
             LT_CAPEX_STAT     LIKE STANDARD TABLE OF LS_CAPEX,
             LT_CT_DATA        LIKE STANDARD TABLE OF LS_CAPEX,
             LS_CAPEX2         LIKE LS_CAPEX,
             LT_CATEGORY       LIKE SORTED TABLE OF LS_CATEGORY WITH UNIQUE KEY ID,
             LT_ASSET_CLASS    LIKE SORTED TABLE OF LS_ASSET_CLASS WITH UNIQUE KEY ID,
             LT_SEL            TYPE UJ0_T_SEL,
             LS_SEL            TYPE UJ0_S_SEL,
             LS_DATA           TYPE UJA_S_DIM_MEMBER,
             LAST_PERIOD       TYPE UJ_DIM_MEMBER, "LAST PERIOD IN THE TIMESCOPE
             LAST_PERIOD_PLUS  TYPE UJ_DIM_MEMBER, "LAST PERIOD IN THE TIMESCOPE_PLUS
             BUDGET_YEAR       TYPE UJA_S_DIM_MEMBER,
             LO_TOOLS          TYPE REF TO ZCL_BPC_TOOLS_CLASSIC, "INSTANCE REFERENCE
             L_LOG             TYPE STRING,
             LS_MESSAGE        TYPE UJ0_S_MESSAGE,
             COUNTER           TYPE UJ_SMALLINT,
             LT_ASSETS_EXT     TYPE UJA_T_DIM_MEMBER,
             LT_STATACCT       TYPE UJA_T_DIM_MEMBER, "CONTAINS CHILDREN OF STATACCT ASSET_CLASS
             WA_STATACCT       TYPE UJ_DIM_MEMBER, "LIKE LINE OF LT_STATACCT,
             WA_MEMBER         TYPE UJ_DIM_MEMBER,
             ORIG_ASSET        TYPE UJ_DIM_MEMBER,
             ORIG_ASSET_CLASS  TYPE UJ_DIM_MEMBER,
             ORIG_AUDITTRAIL   TYPE UJ_DIM_MEMBER,
             ORIG_CATEGORY     TYPE UJ_DIM_MEMBER,
             ORIG_ENTITY       TYPE UJ_DIM_MEMBER,
             ORIG_FLOW         TYPE UJ_DIM_MEMBER,
             ORIG_CURRENCY     TYPE UJ_DIM_MEMBER,
             ORIG_TIME         TYPE UJ_DIM_MEMBER,
             AC_FLOW           TYPE UJ_DIM_MEMBER,
             AC_FLOW_VAT       TYPE UJ_DIM_MEMBER,
             AC_BS_COSTACC     TYPE UJ_DIM_MEMBER,
             AC_BS_DEPAMORT    TYPE UJ_DIM_MEMBER,
             AC_PL_GLACC       TYPE UJ_DIM_MEMBER,
             AC_TRADE_CRED     TYPE UJ_DIM_MEMBER,
             VAR_CP001         TYPE INTEGER,
             VAR_ORIGAMOUNT    TYPE UJ_SDATA,
             VAR_DEBITVAT      TYPE UJ_SDATA,
             VAR_QUOTIENT      TYPE UJ_SDATA,
             TIME_STRING       TYPE STRING,
             TIME_CP001        TYPE STRING,
             TIME_CP003        TYPE STRING,
             YEAR              TYPE CHAR32,
             PERIOD            TYPE C LENGTH 2,
             IYEAR             TYPE INT4,
             IPERIOD           TYPE N LENGTH 2,
             ITIMEID           TYPE I,
             TIME_CHAR         TYPE CHAR32.

    TYPES: BEGIN OF IT_CV_RANGE,
             SIGN(1),
             OPTION(2),
             LOW       TYPE CHAR32,
             HIGH      TYPE CHAR32,
           END OF IT_CV_RANGE.

    DATA: LS_TIME_RANGE TYPE IT_CV_RANGE,
          LT_TIME_RANGE TYPE STANDARD TABLE OF IT_CV_RANGE.


*************************************************************
    "CHECK IT_CV FOR CURRENT VIEW THIS WILL CONTAIN CATEGORY WITH ONLY 1 VALUE.
*** Read IT_CV values
*** SET TOOLS INSTANCE *****
    CREATE OBJECT LO_TOOLS
      EXPORTING
        I_ENV_ID = I_APPSET_ID
        I_MOD_ID = I_APPL_ID.

****** SUB SPECIFIC VARIABLES EXTENDED HERE **********************************
    DATA: FLOWS    TYPE UJA_T_DIM_MEMBER,
          FLOWS2   TYPE UJA_T_DIM_MEMBER, "additional flows read
          LT_FLOWS TYPE UJA_T_DIM_MEMBER,
          ACCOUNTS TYPE UJA_T_DIM_MEMBER.

    DATA: CV_AUDITTRAIL     TYPE UJK_S_CV,
          CV_RPTCURRENCY    TYPE UJK_S_CV,
          CV_CATEGORY       TYPE UJK_S_CV,
          CV_FLOW           TYPE UJK_S_CV,
          CV_ASSET          TYPE UJK_S_CV,
          CV_ASSET_CLASS    TYPE UJK_S_CV,
          CV_ENTITY         TYPE UJK_S_CV,
          LT_CV_ASSET       TYPE UJA_T_DIM_MEMBER,
          LT_CV_ASSET_CLASS TYPE UJA_T_DIM_MEMBER,
          LT_CV_ENTITY      TYPE UJA_T_DIM_MEMBER,
          WORKING_CATEGORY  TYPE UJ_DIM_MEMBER, "BUDGET OR WFORECAST
          CATEGORY_YEAR     TYPE CHAR4 , "UJ_DIM_MEMBER, "YEAR INDICATED IN LT_CATEGORY
          CATEGORY_YEAR2    TYPE CHAR4 , "UJ_DIM_MEMBER, "YEAR INDICATED IN LT_CATEGORY
          YEAR2_INP         TYPE UJ_DIM_MEMBER,
          YEAR_INP          TYPE UJ_DIM_MEMBER.

    CLEAR: WA_MEMBER,CV_CATEGORY,CV_AUDITTRAIL,CV_RPTCURRENCY.
    READ TABLE IT_CV INTO CV_AUDITTRAIL WITH TABLE KEY DIM_UPPER_CASE = 'AUDITTRAIL'.
    "ONLY PROCESS INPUT
    LOOP AT CV_AUDITTRAIL-MEMBER INTO WA_MEMBER.
      IF WA_MEMBER <> 'INPUT'.
        CLEAR: ET_MESSAGE, L_LOG.
        L_LOG = 'AUDITTRAIL MUST BE INPUT '.
        CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).
*     ls_message-msgid = 'BPC' .*     ls_message-msgty = 'I'.*     ls_message-msgno = '010' .
*     ls_message-message = 'More than one Category is being sent!'.*     APPEND ls_message TO et_message.
        EXIT.
      ENDIF.
    ENDLOOP.

    READ TABLE IT_CV INTO CV_RPTCURRENCY WITH TABLE KEY DIM_UPPER_CASE = 'RPTCURRENCY'.
    "ONLY PROCESS INPUT
    LOOP AT CV_RPTCURRENCY-MEMBER INTO WA_MEMBER.
      IF WA_MEMBER <> 'LC'.
        CLEAR: ET_MESSAGE, L_LOG.
        L_LOG = 'CURRENCY MUST BE LC ONLY'.
        CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).
*     ls_message-msgid = 'BPC' .*     ls_message-msgty = 'I'.*     ls_message-msgno = '010' .
*     ls_message-message = 'More than one Category is being sent!'.*     APPEND ls_message TO et_message.
        EXIT.
      ENDIF.
    ENDLOOP.

    "READ IT_CV FOR ASSET AND ENTITY ; ASSET_CLASS TO BE USED IN FLAG TABLES
    READ TABLE IT_CV INTO CV_ASSET WITH TABLE KEY DIM_UPPER_CASE = 'ASSET'.
    "ONLY PROCESS INPUT
    LOOP AT CV_ASSET-MEMBER INTO WA_MEMBER.
      APPEND WA_MEMBER TO LT_CV_ASSET.
    ENDLOOP.

    READ TABLE IT_CV INTO CV_ASSET_CLASS WITH TABLE KEY DIM_UPPER_CASE = 'ASSET_CLASS'.
    "ONLY PROCESS INPUT
    LOOP AT CV_ASSET_CLASS-MEMBER INTO WA_MEMBER.
      APPEND WA_MEMBER TO LT_CV_ASSET_CLASS.
    ENDLOOP.

    READ TABLE IT_CV INTO CV_ENTITY WITH TABLE KEY DIM_UPPER_CASE = 'ENTITY'.
    "ONLY PROCESS INPUT
    LOOP AT CV_ENTITY-MEMBER INTO WA_MEMBER.
      APPEND WA_MEMBER TO LT_CV_ENTITY.
    ENDLOOP.

*******
    READ TABLE IT_CV INTO CV_CATEGORY WITH TABLE KEY DIM_UPPER_CASE = 'CATEGORY'.
**** Cant process two categoroes simultaneously due to time dim restrictions in WFORECAST and BUDGET
    LOOP AT CV_CATEGORY-MEMBER INTO WA_MEMBER.
      IF SY-TABIX > 1.
        CLEAR: ET_MESSAGE, L_LOG.
        L_LOG = 'DO NOT SEND MULTIPLE CATEGORIES SIMULTANEOUSLY! RESTRICT IN PACKAGE OR IN INPUT SCHEDULE.'.
        CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).
*     ls_message-msgid = 'BPC' .*     ls_message-msgty = 'I'.*     ls_message-msgno = '010' .
*     ls_message-message = 'More than one Category is being sent!'.*     APPEND ls_message TO et_message.
        EXIT.
      ENDIF.
    ENDLOOP.
    WORKING_CATEGORY = WA_MEMBER.


******************* GET THE COMMON CAPEX "VARIABLES" *********
*  SUCH AS TIMESCOPE - FORECAST where PROPERY CURRMONTH = C,F in TIME dimension
*  BUDGET - where PROPERY CURRMONTH = B in TIME definition
**************************************************************
    DATA: MASTER_DATA   TYPE REF TO DATA.
    FIELD-SYMBOLS: <DIMENSION_DATA> TYPE STANDARD TABLE.


***** GET ASSET_CLASS REFERENCE *************************************
    CLEAR LS_SEL.
    LS_SEL-DIMENSION = 'ASSET_CLASS'.
    "APPEND LS_SEL TO LT_SEL.

    LO_TOOLS->READ_MASTER_DATA(
      EXPORTING
         I_DIMENSION_ID   = LS_SEL-DIMENSION
      IMPORTING
        OUTPUT_R_DATA = MASTER_DATA
      CHANGING
        LT_SEL        = LT_SEL ).

    ASSIGN MASTER_DATA->* TO <DIMENSION_DATA>.
    MOVE-CORRESPONDING <DIMENSION_DATA> TO LT_ASSET_CLASS.
*******************************************************************
***** GET CATEGORY REFERENCE  *************************************
    CLEAR: LS_SEL, LT_SEL.

    LS_SEL-DIMENSION = 'CATEGORY'.
    LS_SEL-ATTRIBUTE = 'ID'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = 'BUDGET' .
    APPEND LS_SEL TO LT_SEL.
    LS_SEL-LOW = 'WFORECAST' .
    APPEND LS_SEL TO LT_SEL.


    LO_TOOLS->READ_MASTER_DATA(
      EXPORTING
        I_DIMENSION_ID   = LS_SEL-DIMENSION
      IMPORTING
        OUTPUT_R_DATA = MASTER_DATA
      CHANGING
        LT_SEL        = LT_SEL ).

    ASSIGN MASTER_DATA->* TO <DIMENSION_DATA>.
    MOVE-CORRESPONDING <DIMENSION_DATA> TO LT_CATEGORY.

***** GET CHILDREN OF TOTAL_STATACCT  **************************
    CLEAR LS_SEL.
    LS_SEL-DIMENSION = 'ASSET_CLASS'.

    LT_STATACCT = LO_TOOLS->GET_PARENT_MD(
      EXPORTING
      PARENT_MBR = 'TOTAL_STATACCT'
      I_DIMENSION_ID = LS_SEL-DIMENSION ).

    SORT LT_STATACCT .

***** GET CHILDREN OF ASSETS_EXT **************************
    CLEAR LS_SEL.
    LS_SEL-DIMENSION = 'ASSET'.

    CALL METHOD ZCL_BPC_TOOLS_CLASSIC=>READ_MASTER_DATA_HIERARCHY
      EXPORTING
        I_APPSET_ID  = I_APPSET_ID
        I_DIMENSION  = LS_SEL-DIMENSION
        I_PARENT_MBR = 'TOTAL_ASSETS_EXT'
      IMPORTING
        OUTPUT_DATA  = LT_ASSETS_EXT.

    SORT LT_ASSETS_EXT.

**************************************************************
    "GET CATEGORY YEAR FROM LT_CATEGORY
    READ TABLE LT_CATEGORY INTO LS_CATEGORY
    WITH KEY
    ID = WORKING_CATEGORY.
    IF SY-SUBRC = 0.
      CATEGORY_YEAR = LS_CATEGORY-YEAR.
      CATEGORY_YEAR2 = CATEGORY_YEAR + 1.
      YEAR2_INP  =  CATEGORY_YEAR2 && '.INP' .
      YEAR_INP = LS_CATEGORY-YEAR && '.INP' .
    ELSE.
      CLEAR: ET_MESSAGE, L_LOG.
      L_LOG = 'CATEGORY HAS NO YEAR MAINTAINED'.
      CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).
      EXIT.
    ENDIF.

***** GET TIME REFERENCE *************************************
    CLEAR:  LS_SEL, LT_SEL.
    LS_SEL-DIMENSION = 'TIME'.
    LS_SEL-ATTRIBUTE = 'YEAR'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'BT'.
    LS_SEL-LOW = CATEGORY_YEAR.
    LS_SEL-HIGH = CATEGORY_YEAR2.
    APPEND LS_SEL TO LT_SEL.

    LO_TOOLS->READ_MASTER_DATA(
      EXPORTING
        I_DIMENSION_ID   = LS_SEL-DIMENSION
      IMPORTING
        OUTPUT_R_DATA = MASTER_DATA
      CHANGING
        LT_SEL        = LT_SEL ).

    ASSIGN MASTER_DATA->* TO <DIMENSION_DATA>.
    MOVE-CORRESPONDING <DIMENSION_DATA> TO LT_TIMESCOPE.

    "Assign TimeScope
    DELETE LT_TIMESCOPE WHERE ID+5(2) = 'IN' ."OR CURRMONTH = 'A'.
    DELETE LT_TIMESCOPE WHERE CURRMONTH = '' AND BUDMONTH = ''.

    CLEAR: LS_SEL, LT_SEL.
****    "SET TIMESCOPE AS PER CATEGORY REQUIREMENTS
****    LOOP AT LT_TIMESCOPE INTO LS_TIME.
****      IF WORKING_CATEGORY = 'WFORECAST'.
****        IF LS_TIME-BUDMONTH = 'B' AND LS_TIME-CURRMONTH <> 'F'
****          AND LS_TIME-CURRMONTH <> 'C'.
****          DELETE LT_TIMESCOPE INDEX SY-TABIX.
****        ENDIF.
****      ELSEIF WORKING_CATEGORY = 'BUDGET'.
****        IF LS_TIME-BUDMONTH <> 'B'.
****          DELETE LT_TIMESCOPE INDEX SY-TABIX.
****        ENDIF.
****      ENDIF.
****    ENDLOOP.

    SORT LT_TIMESCOPE BY ID DESCENDING.
    READ TABLE LT_TIMESCOPE INDEX 1 INTO LS_TIME.
    LAST_PERIOD = LS_TIME-ID. "FORMAT IN YYYY.MM

    SORT LT_TIMESCOPE BY ID.

    "EXTEND TIME SCOPE PLUS
    CLEAR LT_TIMESCOPE_PLUS.
    LT_TIMESCOPE_PLUS[] = LT_TIMESCOPE[].

    ITIMEID = LAST_PERIOD(4) && LAST_PERIOD+5(2).
    TIME_STRING = LAST_PERIOD(4) && LAST_PERIOD+5(2).
    CLEAR LS_TIME.
    DO 12 TIMES.
      IF TIME_STRING+4(2) = 12.
        ITIMEID = ITIMEID + 89 .
      ELSE.
        ITIMEID = ITIMEID + 1 .
      ENDIF.
      TIME_STRING = ITIMEID.
      LS_TIME-ID = TIME_STRING(4) && '.' && TIME_STRING+4(2).
      LS_TIME-CURRMONTH = 'Z'.
      CLEAR LS_TIME-BUDMONTH  .
      TIME_STRING = TIME_STRING && '00'.
      CONDENSE TIME_STRING NO-GAPS.
      LS_TIME-TIMEID = TIME_STRING." TIME_STRING && '00'.
      LS_TIME-YEAR = TIME_STRING(4).
      LAST_PERIOD_PLUS = LS_TIME-ID.
      APPEND LS_TIME TO LT_TIMESCOPE_PLUS.
    ENDDO.

********************* DATA VARIABLES *************************
    DATA: LT_FINAL      TYPE REF TO DATA,
          LT_CPXXX      TYPE REF TO DATA,
          LS_REC        TYPE REF TO DATA,
          LS_RESULT_REC TYPE REF TO DATA.

    FIELD-SYMBOLS: <LT_FINAL>      TYPE STANDARD TABLE,
                   <LS_REC>        TYPE ANY,
                   <LS_RESULT_REC> TYPE ANY.

    FIELD-SYMBOLS: <LS_ASSET>       TYPE ANY,
                   <LS_ASSET_CLASS> TYPE ANY,
                   <LS_AUDITTRAIL>  TYPE ANY,
                   <LS_CATEGORY>    TYPE ANY,
                   <LS_ENTITY>      TYPE ANY,
                   <LS_FLOW>        TYPE ANY,
                   <LS_RPTCURRENCY> TYPE ANY,
                   <LS_TIME>        TYPE ANY,
                   <LS_SIGNEDDATA>  TYPE ANY.
**********************************************************************
************************ASSIGN STRUCTURE OF INCOMING MODEL ***********
    DATA:  LT_DIM_LIST    TYPE UJA_T_DIM_LIST,
           LO_APPL_MGR    TYPE REF TO IF_UJA_APPLICATION_MANAGER,
           LO_QUERY       TYPE REF TO IF_UJO_QUERY,
           LR_DATA        TYPE REF TO DATA,
           LS_APPLICATION TYPE UJA_S_APPLICATION,
           LS_DIMENSIONS  TYPE UJA_S_DIMENSION,
           LT_MESSAGE     TYPE UJ0_T_MESSAGE.
    FIELD-SYMBOLS: <CT_DATA>   TYPE  STANDARD TABLE,
                   <DATA_LINE> TYPE ANY.

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

    ASSIGN LR_DATA->* TO <CT_DATA>.

************ ASSIGN WAs ***************************************************
    CREATE DATA LT_FINAL LIKE CT_DATA.
    CREATE DATA LT_CPXXX LIKE CT_DATA.
    ASSIGN LT_FINAL->* TO <LT_FINAL>.
*    ASSIGN LT_FINAL->* TO <CT_DATA>.
    CREATE DATA LS_RESULT_REC LIKE LINE OF CT_DATA.
    ASSIGN LS_RESULT_REC->* TO <LS_RESULT_REC>.
    CREATE DATA LS_REC LIKE LINE OF CT_DATA.
    ASSIGN LS_REC->* TO <LS_REC>.
***************************************************************************
*************READ CAPEX CUBE FOR THE STAT ACCOUNTS TO BE CLEARED **********
    "CAPEX TABLE WITH STATACCTS BASIS FOR CLEARING
    CLEAR: LT_SEL, LS_SEL.

    LS_SEL-DIMENSION = 'FLOW'.
    LS_SEL-ATTRIBUTE = 'ID'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'NE'.
    LS_SEL-LOW = 'CP005'.
    APPEND LS_SEL TO LT_SEL.

    LS_SEL-DIMENSION = 'FLOW'.
    LS_SEL-ATTRIBUTE = 'ID'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'NE'.
    LS_SEL-LOW = 'CP006'.
    APPEND LS_SEL TO LT_SEL.

    LS_SEL-DIMENSION = 'RPTCURRENCY'.
    LS_SEL-ATTRIBUTE = 'ID'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = 'LC'.
    APPEND LS_SEL TO LT_SEL.

    LS_SEL-DIMENSION = 'AUDITTRAIL'.
    LS_SEL-ATTRIBUTE = 'ID'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = 'INPUT'.
    APPEND LS_SEL TO LT_SEL.

    LS_SEL-DIMENSION = 'CATEGORY'.
    LS_SEL-ATTRIBUTE = 'ID'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = WORKING_CATEGORY. "'BUDGET'.
    APPEND LS_SEL TO LT_SEL.

    CLEAR WA_STATACCT.
    LOOP AT LT_STATACCT INTO WA_STATACCT.
      LS_SEL-DIMENSION = 'ASSET_CLASS'.
      LS_SEL-ATTRIBUTE = 'ID'.
      LS_SEL-SIGN = 'I'.
      LS_SEL-OPTION = 'EQ'.
      LS_SEL-LOW = WA_STATACCT.
      APPEND LS_SEL TO LT_SEL.
    ENDLOOP.

****    LOOP AT LT_CV_ASSET INTO WA_MEMBER.
****      LS_SEL-DIMENSION = 'ASSET'.
****      LS_SEL-ATTRIBUTE = 'ID'.
****      LS_SEL-SIGN = 'I'.
****      LS_SEL-OPTION = 'EQ'.
****      LS_SEL-LOW = WA_MEMBER.
****      APPEND LS_SEL TO LT_SEL.
****    ENDLOOP.

    LOOP AT LT_CV_ENTITY INTO WA_MEMBER.
      LS_SEL-DIMENSION = 'ENTITY'.
      LS_SEL-ATTRIBUTE = 'ID'.
      LS_SEL-SIGN = 'I'.
      LS_SEL-OPTION = 'EQ'.
      LS_SEL-LOW = WA_MEMBER.
      APPEND LS_SEL TO LT_SEL.
    ENDLOOP.

    IF WORKING_CATEGORY = 'WFORECAST'.
      CLEAR LS_TIME.
      CLEAR LS_TIME_RANGE.
      LOOP AT LT_TIMESCOPE_PLUS INTO LS_TIME.
        LS_SEL-DIMENSION = 'TIME'.
        LS_SEL-ATTRIBUTE = 'ID'.
        LS_SEL-SIGN = 'I'.
        LS_SEL-OPTION = 'EQ'.
        LS_SEL-LOW = LS_TIME-ID.
        APPEND LS_SEL TO LT_SEL.
        LS_TIME_RANGE-SIGN = 'I'.
        LS_TIME_RANGE-OPTION = 'EQ'.
        LS_TIME_RANGE-LOW = LS_TIME-ID.
        APPEND LS_TIME_RANGE TO LT_TIME_RANGE.
      ENDLOOP.
    ELSE. "BUDGET
      CLEAR LS_TIME.
      CLEAR LS_TIME_RANGE.
      LOOP AT LT_TIMESCOPE INTO LS_TIME.
        LS_SEL-DIMENSION = 'TIME'.
        LS_SEL-ATTRIBUTE = 'ID'.
        LS_SEL-SIGN = 'I'.
        LS_SEL-OPTION = 'EQ'.
        LS_SEL-LOW = LS_TIME-ID.
        APPEND LS_SEL TO LT_SEL.
        LS_TIME_RANGE-SIGN = 'I'.
        LS_TIME_RANGE-OPTION = 'EQ'.
        LS_TIME_RANGE-LOW = LS_TIME-ID.
        APPEND LS_TIME_RANGE TO LT_TIME_RANGE.
      ENDLOOP.
    ENDIF.

    LO_TOOLS->READ_MODEL_DATA(
      EXPORTING
        IT_SEL      = LT_SEL
      IMPORTING
        OUTPUT_DATA = <CT_DATA> ).

    CLEAR LT_CAPEX_STAT.
    MOVE-CORRESPONDING <CT_DATA> TO LT_CAPEX_STAT.
    SORT LT_CAPEX_STAT BY ENTITY ASSET_CLASS ASSET FLOW TIME.

*************READ CAPEX CUBE FOR THE YYYY.INP BASED ON CATEGORY YEAR AND CATEGORY**********
    "FLAG TABLE WILL ONLY EVER HAVE YEAR.INP VALUE IN THE TIME
    CLEAR: LT_SEL, LS_SEL.

***    LOOP AT LT_CV_ASSET INTO WA_MEMBER.
***      LS_SEL-DIMENSION = 'ASSET'.
***      LS_SEL-ATTRIBUTE = 'ID'.
***      LS_SEL-SIGN = 'I'.
***      LS_SEL-OPTION = 'EQ'.
***      LS_SEL-LOW = WA_MEMBER.
***      APPEND LS_SEL TO LT_SEL.
***    ENDLOOP.

    LOOP AT LT_CV_ENTITY INTO WA_MEMBER.
      LS_SEL-DIMENSION = 'ENTITY'.
      LS_SEL-ATTRIBUTE = 'ID'.
      LS_SEL-SIGN = 'I'.
      LS_SEL-OPTION = 'EQ'.
      LS_SEL-LOW = WA_MEMBER.
      APPEND LS_SEL TO LT_SEL.
    ENDLOOP.

    LS_SEL-DIMENSION = 'RPTCURRENCY'.
    LS_SEL-ATTRIBUTE = 'ID'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = 'LC'.
    APPEND LS_SEL TO LT_SEL.

    LS_SEL-DIMENSION = 'AUDITTRAIL'.
    LS_SEL-ATTRIBUTE = 'ID'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = 'INPUT'.
    APPEND LS_SEL TO LT_SEL.

    LS_SEL-DIMENSION = 'CATEGORY'.
    LS_SEL-ATTRIBUTE = 'ID'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = WORKING_CATEGORY. "'BUDGET'.
    APPEND LS_SEL TO LT_SEL.

    LS_SEL-DIMENSION = 'TIME'.
    LS_SEL-ATTRIBUTE = 'ID'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = CATEGORY_YEAR && '.INP' ."'2018.INP'.
    APPEND LS_SEL TO LT_SEL.

    LO_TOOLS->READ_MODEL_DATA(
      EXPORTING
        IT_SEL      = LT_SEL
      IMPORTING
        OUTPUT_DATA = <CT_DATA> ).

    CLEAR LT_CAPEX_FLG.
    MOVE-CORRESPONDING <CT_DATA> TO LT_CAPEX_FLG.
    SORT LT_CAPEX_FLG BY ENTITY ASSET_CLASS ASSET FLOW TIME.

************************************************************************************
*************READ CAPEX CUBE FOR THE YYYY.MMM BASED ON CATEGORY AND TIMESCOPE**********
********"TRANSACTIONS TABLE
********    CLEAR: LT_SEL, LS_SEL.
********
***********    LOOP AT LT_CV_ASSET INTO WA_MEMBER.
***********      LS_SEL-DIMENSION = 'ASSET'.
***********      LS_SEL-ATTRIBUTE = 'ID'.
***********      LS_SEL-SIGN = 'I'.
***********      LS_SEL-OPTION = 'EQ'.
***********      LS_SEL-LOW = WA_MEMBER.
***********      APPEND LS_SEL TO LT_SEL.
***********    ENDLOOP.
********
********    LOOP AT LT_CV_ENTITY INTO WA_MEMBER.
********      LS_SEL-DIMENSION = 'ENTITY'.
********      LS_SEL-ATTRIBUTE = 'ID'.
********      LS_SEL-SIGN = 'I'.
********      LS_SEL-OPTION = 'EQ'.
********      LS_SEL-LOW = WA_MEMBER.
********      APPEND LS_SEL TO LT_SEL.
********    ENDLOOP.
********
********    LS_SEL-DIMENSION = 'RPTCURRENCY'.
********    LS_SEL-ATTRIBUTE = 'ID'.
********    LS_SEL-SIGN = 'I'.
********    LS_SEL-OPTION = 'EQ'.
********    LS_SEL-LOW = 'LC'.
********    APPEND LS_SEL TO LT_SEL.
********
********
********    LS_SEL-DIMENSION = 'AUDITTRAIL'.
********    LS_SEL-ATTRIBUTE = 'ID'.
********    LS_SEL-SIGN = 'I'.
********    LS_SEL-OPTION = 'EQ'.
********    LS_SEL-LOW = 'INPUT'.
********    APPEND LS_SEL TO LT_SEL.
********
********    LS_SEL-DIMENSION = 'CATEGORY'.
********    LS_SEL-ATTRIBUTE = 'ID'.
********    LS_SEL-SIGN = 'I'.
********    LS_SEL-OPTION = 'EQ'.
********    LS_SEL-LOW = WORKING_CATEGORY.
********    APPEND LS_SEL TO LT_SEL.
********
********IF WORKING_CATEGORY = 'WFORECAST'.
********    CLEAR LS_TIME.
********    CLEAR LS_TIME_RANGE.
********    LOOP AT LT_TIMESCOPE_PLUS INTO LS_TIME.
********      LS_SEL-DIMENSION = 'TIME'.
********      LS_SEL-ATTRIBUTE = 'ID'.
********      LS_SEL-SIGN = 'I'.
********      LS_SEL-OPTION = 'EQ'.
********      LS_SEL-LOW = LS_TIME-ID.
********      APPEND LS_SEL TO LT_SEL.
********      LS_TIME_RANGE-SIGN = 'I'.
********      LS_TIME_RANGE-OPTION = 'EQ'.
********      LS_TIME_RANGE-LOW = LS_TIME-ID.
********      APPEND LS_TIME_RANGE TO LT_TIME_RANGE.
********    ENDLOOP.
********ELSE. "BUDGET
********    CLEAR LS_TIME.
********    CLEAR LS_TIME_RANGE.
********    LOOP AT LT_TIMESCOPE INTO LS_TIME.
********      LS_SEL-DIMENSION = 'TIME'.
********      LS_SEL-ATTRIBUTE = 'ID'.
********      LS_SEL-SIGN = 'I'.
********      LS_SEL-OPTION = 'EQ'.
********      LS_SEL-LOW = LS_TIME-ID.
********      APPEND LS_SEL TO LT_SEL.
********      LS_TIME_RANGE-SIGN = 'I'.
********      LS_TIME_RANGE-OPTION = 'EQ'.
********      LS_TIME_RANGE-LOW = LS_TIME-ID.
********      APPEND LS_TIME_RANGE TO LT_TIME_RANGE.
********    ENDLOOP.
********  ENDIF.
********
********    LS_SEL-DIMENSION = 'FLOW'.
********    LS_SEL-ATTRIBUTE = 'ID'.
********    LS_SEL-SIGN = 'I'.
********    LS_SEL-OPTION = 'EQ'.
********    LS_SEL-LOW = 'CP005'.
********    APPEND LS_SEL TO LT_SEL.
********
********    LO_TOOLS->READ_MODEL_DATA(
********      EXPORTING
********        IT_SEL      = LT_SEL
********      IMPORTING
********        OUTPUT_DATA = <CT_DATA> ).
********
********    CLEAR LT_CAPEX_TX.
********    MOVE-CORRESPONDING <CT_DATA> TO LT_CAPEX_TX.
********    SORT LT_CAPEX_TX BY ENTITY ASSET ASSET_CLASS FLOW TIME.

***********************************************************************************
    "---EVALUATE CT_DATA FOR FLAGS AND ADD CORRESPONDING DATA INTO CT_DATA
    "---THIS IS DONE SO THAT WE INCLUDED DATA INTO CT_DATA WHERE THE FLAG PERTAINS TO
    "FETCH CP005 DATA OF WORKING CATEGORY , LC  , INPUT FOR EVERY ENTITY IN THE CT_DATA
    "ACROSS THE TIME SCOPE

    CLEAR: LT_SEL, LS_SEL.

    LS_SEL-DIMENSION = 'FLOW'.
    LS_SEL-ATTRIBUTE = 'ID'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = 'CP005'.
    APPEND LS_SEL TO LT_SEL.

    LS_SEL-DIMENSION = 'FLOW'.
    LS_SEL-ATTRIBUTE = 'ID'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = 'CP006'.
    APPEND LS_SEL TO LT_SEL.
    "TRIAL -START
    LS_SEL-DIMENSION = 'FLOW'.
    LS_SEL-ATTRIBUTE = 'ID'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = 'CP102'.
    APPEND LS_SEL TO LT_SEL.

    LS_SEL-DIMENSION = 'TIME'.
    LS_SEL-ATTRIBUTE = 'ID'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = YEAR_INP.
    APPEND LS_SEL TO LT_SEL.

    "TRIAL -END
    LS_SEL-DIMENSION = 'RPTCURRENCY'.
    LS_SEL-ATTRIBUTE = 'ID'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = 'LC'.
    APPEND LS_SEL TO LT_SEL.

    LS_SEL-DIMENSION = 'AUDITTRAIL'.
    LS_SEL-ATTRIBUTE = 'ID'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = 'INPUT'.
    APPEND LS_SEL TO LT_SEL.

    LS_SEL-DIMENSION = 'CATEGORY'.
    LS_SEL-ATTRIBUTE = 'ID'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = WORKING_CATEGORY.
    APPEND LS_SEL TO LT_SEL.

    LOOP AT LT_CV_ENTITY INTO WA_MEMBER.
      LS_SEL-DIMENSION = 'ENTITY'.
      LS_SEL-ATTRIBUTE = 'ID'.
      LS_SEL-SIGN = 'I'.
      LS_SEL-OPTION = 'EQ'.
      LS_SEL-LOW = WA_MEMBER.
      APPEND LS_SEL TO LT_SEL.
    ENDLOOP.

    CLEAR LS_TIME.
    LOOP AT LT_TIMESCOPE INTO LS_TIME.
      LS_SEL-DIMENSION = 'TIME'.
      LS_SEL-ATTRIBUTE = 'ID'.
      LS_SEL-SIGN = 'I'.
      LS_SEL-OPTION = 'EQ'.
      LS_SEL-LOW = LS_TIME-ID.
      APPEND LS_SEL TO LT_SEL.
    ENDLOOP.

    IF WORKING_CATEGORY = 'WFORECAST'.
      CLEAR LS_TIME.
      CLEAR LS_TIME_RANGE.
      LOOP AT LT_TIMESCOPE_PLUS INTO LS_TIME.
        LS_SEL-DIMENSION = 'TIME'.
        LS_SEL-ATTRIBUTE = 'ID'.
        LS_SEL-SIGN = 'I'.
        LS_SEL-OPTION = 'EQ'.
        LS_SEL-LOW = LS_TIME-ID.
        APPEND LS_SEL TO LT_SEL.
        LS_TIME_RANGE-SIGN = 'I'.
        LS_TIME_RANGE-OPTION = 'EQ'.
        LS_TIME_RANGE-LOW = LS_TIME-ID.
        APPEND LS_TIME_RANGE TO LT_TIME_RANGE.
      ENDLOOP.
    ELSE. "BUDGET
      CLEAR LS_TIME.
      CLEAR LS_TIME_RANGE.
      LOOP AT LT_TIMESCOPE INTO LS_TIME.
        LS_SEL-DIMENSION = 'TIME'.
        LS_SEL-ATTRIBUTE = 'ID'.
        LS_SEL-SIGN = 'I'.
        LS_SEL-OPTION = 'EQ'.
        LS_SEL-LOW = LS_TIME-ID.
        APPEND LS_SEL TO LT_SEL.
        LS_TIME_RANGE-SIGN = 'I'.
        LS_TIME_RANGE-OPTION = 'EQ'.
        LS_TIME_RANGE-LOW = LS_TIME-ID.
        APPEND LS_TIME_RANGE TO LT_TIME_RANGE.
      ENDLOOP.
    ENDIF.

    LO_TOOLS->READ_MODEL_DATA(
  EXPORTING
    IT_SEL      = LT_SEL
  IMPORTING
    OUTPUT_DATA = <CT_DATA> ).

    CLEAR LT_CT_DATA.
    MOVE-CORRESPONDING <CT_DATA> TO LT_CT_DATA.
    SORT LT_CT_DATA BY ENTITY ASSET_CLASS ASSET FLOW TIME.
    "ASSET ASSET_CLASS AUDITTRAIL ENTITY FLOW CATEGORY TIME RPTCURRENCY.
    DELETE ADJACENT DUPLICATES FROM LT_CT_DATA COMPARING
    ENTITY ASSET_CLASS ASSET FLOW TIME.
    "ASSET ASSET_CLASS AUDITTRAIL ENTITY FLOW CATEGORY TIME RPTCURRENCY.

    CLEAR CT_DATA.
    MOVE-CORRESPONDING LT_CT_DATA TO CT_DATA.

"ADJUST TIME_SCOPE DEPENDING ON WORKING CATEGORY
IF WORKING_CATEGORY = 'WFORECAST'.
  "USE TIMESCOPE PLUS
  CLEAR LT_TIMESCOPE.
  LT_TIMESCOPE[] = LT_TIMESCOPE_PLUS[] .
  ENDIF.

************************************************************************************
    "NOW EVALUATE THE NEW CT_DATA FOR CALCS AND EVERYTHING.
    DATA: FLAG1 TYPE CHAR1.
    CLEAR: LT_CAPEX_XXX .

    LOOP AT CT_DATA ASSIGNING <LS_REC>.
      CLEAR FLAG1.
      <LS_RESULT_REC> = <LS_REC>.
      ASSIGN COMPONENT 'ASSET'        OF STRUCTURE <LS_RESULT_REC> TO <LS_ASSET>.
      ASSIGN COMPONENT 'ASSET_CLASS'  OF STRUCTURE <LS_RESULT_REC> TO <LS_ASSET_CLASS>.
      ASSIGN COMPONENT 'AUDITTRAIL'   OF STRUCTURE <LS_RESULT_REC> TO <LS_AUDITTRAIL>.
      ASSIGN COMPONENT 'ENTITY'       OF STRUCTURE <LS_RESULT_REC> TO <LS_ENTITY>.
      ASSIGN COMPONENT 'FLOW'         OF STRUCTURE <LS_RESULT_REC> TO <LS_FLOW>.
      ASSIGN COMPONENT 'CATEGORY'     OF STRUCTURE <LS_RESULT_REC> TO <LS_CATEGORY>.
      ASSIGN COMPONENT 'TIME'         OF STRUCTURE <LS_RESULT_REC> TO <LS_TIME>.
      ASSIGN COMPONENT 'RPTCURRENCY'  OF STRUCTURE <LS_RESULT_REC> TO <LS_RPTCURRENCY>.
      ASSIGN COMPONENT 'SIGNEDDATA'   OF STRUCTURE <LS_RESULT_REC> TO <LS_SIGNEDDATA>.

      ORIG_ASSET         =  <LS_ASSET>.
      ORIG_ASSET_CLASS   =  <LS_ASSET_CLASS>.
      ORIG_AUDITTRAIL    =  <LS_AUDITTRAIL>.
      ORIG_CATEGORY      =  <LS_CATEGORY>.
      ORIG_ENTITY        =  <LS_ENTITY>.
      ORIG_FLOW          =  <LS_FLOW>.
      ORIG_CURRENCY      =  <LS_RPTCURRENCY>.
      ORIG_TIME          =  <LS_TIME>.
      VAR_ORIGAMOUNT     =  <LS_SIGNEDDATA>.

      "GET PROPERTY OF ASSET CLASS - END

      "GET PROPERTY OF ASSET CLASS - START
      CLEAR LS_ASSET_CLASS.
      READ TABLE LT_ASSET_CLASS WITH KEY
      ID = ORIG_ASSET_CLASS
      INTO LS_ASSET_CLASS
      BINARY SEARCH.
      IF SY-SUBRC = 0.
        AC_FLOW         = LS_ASSET_CLASS-FLOW.
        AC_FLOW_VAT     = LS_ASSET_CLASS-FLOW_VAT.
        AC_BS_COSTACC   = LS_ASSET_CLASS-BS_COSTACC.
        AC_BS_DEPAMORT  = LS_ASSET_CLASS-BS_DEPAMORT.
        AC_PL_GLACC     = LS_ASSET_CLASS-PL_GLACC.
        AC_TRADE_CRED   = LS_ASSET_CLASS-TRADE_CRED.
      ENDIF.

      IF AC_FLOW IS INITIAL.
        AC_FLOW         = 'ASSET_CLASS NO FLOW - ' && ORIG_ASSET_CLASS.
      ENDIF.
      IF AC_FLOW_VAT IS INITIAL.
        AC_FLOW_VAT     = 'ASSET_CLASS NO VAT - ' && ORIG_ASSET_CLASS.
      ENDIF.
      IF AC_BS_COSTACC IS INITIAL.
        AC_BS_COSTACC   = 'ASSET_CLASS NO COSTACC - ' && ORIG_ASSET_CLASS.
      ENDIF.
      IF AC_BS_DEPAMORT IS INITIAL.
        AC_BS_DEPAMORT  = 'ASSET_CLASS NO BS_DEPAMORT - ' && ORIG_ASSET_CLASS.
      ENDIF.
      IF AC_TRADE_CRED IS INITIAL.
        AC_TRADE_CRED   = 'ASSET_CLASS NO TRADE_CREDW - ' && ORIG_ASSET_CLASS.
      ENDIF.
      IF AC_PL_GLACC IS INITIAL.
        AC_PL_GLACC     = 'ASSET_CLASS NO TRADE_CREDW - ' && ORIG_ASSET_CLASS.
      ENDIF.

" SUB 2 CLEAR DATA WHERE CP102 = 1 - START
***
***      CASE <LS_CATEGORY>.
***
***        WHEN 'WFORECAST'.
***          "CHECK CP102 FLOW YYYY.INP
***          CLEAR LS_CAPEX.
***          READ TABLE LT_CAPEX_FLG
***          WITH KEY
***          ASSET       = ORIG_ASSET
***          ASSET_CLASS = ORIG_ASSET_CLASS
***          AUDITTRAIL  = ORIG_AUDITTRAIL
***          CATEGORY    = ORIG_CATEGORY
***          ENTITY      = ORIG_ENTITY
***          FLOW        = 'CP102'
***          RPTCURRENCY = ORIG_CURRENCY
***          TIME        = YEAR_INP
***          INTO LS_CAPEX.
***
***          IF LS_CAPEX-SIGNEDDATA = 1."ASSET CREATED
***            "CLEAR DATA FOR THAT ENTITY ASSET_CLASS ASSET CATEGORY LC
***            "CLEAR ALL FLOWS AS WELL
***            "CLEAR THE STAT ACCOUNTS BASED ON THE PROPERTY OF THE ASSET_CLASS
***
***            LOOP AT LT_TIMESCOPE_PLUS INTO LS_TIME.
***              <LS_TIME> = LS_TIME-ID.
***              <LS_SIGNEDDATA> = 0.
***              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
***            ENDLOOP.
***
***
***
***
***
***            APPEND LS_CAPEX TO LT_CAPEX_XXX. "THIS IS FOR CLEARING THE CP102 FLAG
***
***          ELSE. "ASSET NOT CREATED.
***            FLAG1 = 1. "PROCEED TO SUB3 ETC.
***          ENDIF.

***        WHEN 'BUDGET'.
          "CHECK CP102 FLOW YYYY.INP

          CLEAR LS_CAPEX.
          READ TABLE LT_CAPEX_FLG
          WITH KEY
          ASSET       = ORIG_ASSET
          ASSET_CLASS = ORIG_ASSET_CLASS
          AUDITTRAIL  = ORIG_AUDITTRAIL
          CATEGORY    = ORIG_CATEGORY
          ENTITY      = ORIG_ENTITY
          FLOW        = 'CP102'
          RPTCURRENCY = ORIG_CURRENCY
          TIME        = YEAR_INP
          INTO LS_CAPEX.

          IF ORIG_FLOW = 'CP102' .

          IF LS_CAPEX-SIGNEDDATA = 1."ASSET CREATED
            "CLEAR DATA FOR THAT ENTITY ASSET_CLASS ASSET CATEGORY LC

            "CLEARING OF CP005
            LOOP AT LT_TIMESCOPE INTO LS_TIME.
              <LS_TIME> = LS_TIME-ID.
              <LS_SIGNEDDATA> = 0.
              <LS_FLOW> = 'CP005'.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            ENDLOOP.

            "CLEARING OF CP006
            LOOP AT LT_TIMESCOPE INTO LS_TIME.
              <LS_TIME> = LS_TIME-ID.
              <LS_SIGNEDDATA> = 0.
              <LS_FLOW> = 'CP006'.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            ENDLOOP.

            "CLEARING OF BS_COSTACC
            CLEAR LS_ASSET_CLASS.
            READ TABLE LT_ASSET_CLASS WITH KEY
            ID = AC_BS_COSTACC
            INTO LS_ASSET_CLASS
            BINARY SEARCH.
            IF SY-SUBRC = 0.
              AC_FLOW = LS_ASSET_CLASS-FLOW.
            ELSE.
              AC_FLOW = 'ASSET_CLASS NO FLOW - ' && AC_BS_COSTACC.
            ENDIF.
            LOOP AT LT_TIMESCOPE INTO LS_TIME.
              <LS_ASSET_CLASS> = AC_BS_COSTACC.
              <LS_TIME> = LS_TIME-ID.
              <LS_SIGNEDDATA> = 0.
              <LS_FLOW> = AC_FLOW.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            ENDLOOP.

            "CLEARING OF BS_DEPAMORT
            CLEAR LS_ASSET_CLASS.
            READ TABLE LT_ASSET_CLASS WITH KEY
            ID = AC_BS_DEPAMORT
            INTO LS_ASSET_CLASS
            BINARY SEARCH.
            IF SY-SUBRC = 0.
              AC_FLOW = LS_ASSET_CLASS-FLOW.
            ELSE.
              AC_FLOW = 'ASSET_CLASS NO FLOW - ' && AC_BS_DEPAMORT.
            ENDIF.
            LOOP AT LT_TIMESCOPE INTO LS_TIME.
              <LS_ASSET_CLASS> = AC_BS_DEPAMORT.
              <LS_TIME> = LS_TIME-ID.
              <LS_SIGNEDDATA> = 0.
              <LS_FLOW> = AC_FLOW.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            ENDLOOP.

            "CLEARING OF PL_GLACC
            CLEAR LS_ASSET_CLASS.
            READ TABLE LT_ASSET_CLASS WITH KEY
            ID = AC_PL_GLACC
            INTO LS_ASSET_CLASS
            BINARY SEARCH.
            IF SY-SUBRC = 0.
              AC_FLOW = LS_ASSET_CLASS-FLOW.
            ELSE.
              AC_FLOW = 'ASSET_CLASS NO FLOW - ' && AC_PL_GLACC.
            ENDIF.
            LOOP AT LT_TIMESCOPE INTO LS_TIME.
              <LS_ASSET_CLASS> = AC_PL_GLACC.
              <LS_TIME> = LS_TIME-ID.
              <LS_SIGNEDDATA> = 0.
              <LS_FLOW> = AC_FLOW.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            ENDLOOP.

            "CLEARING OF TRADE_CRED
            CLEAR LS_ASSET_CLASS.
            READ TABLE LT_ASSET_CLASS WITH KEY
            ID = AC_TRADE_CRED
            INTO LS_ASSET_CLASS
            BINARY SEARCH.
            IF SY-SUBRC = 0.
              AC_FLOW = LS_ASSET_CLASS-FLOW.
            ELSE.
              AC_FLOW = 'ASSET_CLASS NO FLOW - ' && AC_TRADE_CRED.
            ENDIF.
            LOOP AT LT_TIMESCOPE INTO LS_TIME.
              <LS_ASSET_CLASS> = AC_TRADE_CRED.
              <LS_TIME> = LS_TIME-ID.
              <LS_SIGNEDDATA> = 0.
              <LS_FLOW> = AC_FLOW.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            ENDLOOP.

            "CLEARING OF 'AC219050'
            CLEAR LS_ASSET_CLASS.
            READ TABLE LT_ASSET_CLASS WITH KEY
            ID = 'AC219050'
            INTO LS_ASSET_CLASS
            BINARY SEARCH.
            IF SY-SUBRC = 0.
              AC_FLOW = LS_ASSET_CLASS-FLOW.
            ELSE.
              AC_FLOW = 'ASSET_CLASS NO FLOW - ' && 'AC219050'.
            ENDIF.
            LOOP AT LT_TIMESCOPE INTO LS_TIME.
              <LS_ASSET_CLASS> = 'AC219050'.
              <LS_TIME> = LS_TIME-ID.
              <LS_SIGNEDDATA> = 0.
              <LS_FLOW> = AC_FLOW.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            ENDLOOP.

            "CLEARING OF 'AC310000'
            CLEAR LS_ASSET_CLASS.
            READ TABLE LT_ASSET_CLASS WITH KEY
            ID = 'AC310000'
            INTO LS_ASSET_CLASS
            BINARY SEARCH.
            IF SY-SUBRC = 0.
              AC_FLOW = LS_ASSET_CLASS-FLOW.
            ELSE.
              AC_FLOW = 'ASSET_CLASS NO FLOW - ' && 'AC310000'.
            ENDIF.
            LOOP AT LT_TIMESCOPE INTO LS_TIME.
              <LS_ASSET_CLASS> = 'AC310000'.
              <LS_TIME> = LS_TIME-ID.
              <LS_SIGNEDDATA> = 0.
              <LS_FLOW> = AC_FLOW.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            ENDLOOP.

            "CLEARING OF 'AC300000'
            CLEAR LS_ASSET_CLASS.
            READ TABLE LT_ASSET_CLASS WITH KEY
            ID = 'AC300000'
            INTO LS_ASSET_CLASS
            BINARY SEARCH.
            IF SY-SUBRC = 0.
              AC_FLOW_VAT = LS_ASSET_CLASS-FLOW_VAT.
            ELSE.
              AC_FLOW_VAT = 'ASSET_CLASS NO FLOW_VAT - ' && 'AC300000'.
            ENDIF.
            LOOP AT LT_TIMESCOPE INTO LS_TIME.
              <LS_ASSET_CLASS> = 'AC300000'.
              <LS_TIME> = LS_TIME-ID.
              <LS_SIGNEDDATA> = 0.
              <LS_FLOW> = AC_FLOW_VAT.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            ENDLOOP.

            "CLEARING OF 'AC310000'
            CLEAR LS_ASSET_CLASS.
            READ TABLE LT_ASSET_CLASS WITH KEY
            ID = 'AC882000'
            INTO LS_ASSET_CLASS
            BINARY SEARCH.
            IF SY-SUBRC = 0.
              AC_FLOW = LS_ASSET_CLASS-FLOW.
            ELSE.
              AC_FLOW = 'ASSET_CLASS NO FLOW - ' && 'AC882000'.
            ENDIF.
            LOOP AT LT_TIMESCOPE INTO LS_TIME.
              <LS_ASSET_CLASS> = 'AC882000' .
              <LS_TIME> = LS_TIME-ID.
              <LS_SIGNEDDATA> = 0.
              <LS_FLOW> = AC_FLOW.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            ENDLOOP.



            APPEND LS_CAPEX TO LT_CAPEX_XXX.

          ELSE. "ASSET NOT CREATED.
            FLAG1 = 1.
          ENDIF.

          ENDIF.
***        WHEN OTHERS.
***
***      ENDCASE.

" SUB 2 CLEAR DATA WHERE CP102 = 1 - END
****************************************************************************************
      "EVALUATE FLAG 1 FOR SUB3 TO SUB 5 CALCULATION
***      IF FLAG1 <> 1.
***        CONTINUE.
***      ELSE.
***      ENDIF.

      IF ( <LS_FLOW> = 'CP005' OR <LS_FLOW> = 'CP006' )  AND LS_CAPEX-SIGNEDDATA <>  1 .
        "PROCEEDS EXECUTING THE CODE.
      ELSE.
        "NO ALTERATION NEEDED
        CONTINUE.
      ENDIF.

****************************************************************************************
      "SUB 3 CAPITALISATION  -- START
      "CHECK IF CHILD OF ASSETS_EXT

      "IF FLOW IS CP006 AND ORIG_TIME(4) = CATEGORY_YEAR2 THEN SKIP
      IF <LS_FLOW> = 'CP006'  AND ORIG_TIME(4) = CATEGORY_YEAR2.
        CONTINUE.
      ENDIF.


*CMC001 Change Block Start *************************************************************

      READ TABLE LT_ASSETS_EXT WITH KEY TABLE_LINE = <LS_ASSET> TRANSPORTING NO FIELDS.

      IF SY-SUBRC = 0."Check the asset type - If found = External

*CMC001 Change Block End   *************************************************************

      IF ORIG_FLOW <> 'CP006'.
        <LS_ASSET_CLASS> = 'AC219050'.
        CLEAR LS_ASSET_CLASS.
        READ TABLE LT_ASSET_CLASS WITH KEY
        ID = <LS_ASSET_CLASS>
        INTO LS_ASSET_CLASS
        BINARY SEARCH.
        IF SY-SUBRC = 0.
          AC_FLOW         = LS_ASSET_CLASS-FLOW.
          <LS_FLOW> = AC_FLOW.
        ELSE.
          <LS_FLOW> = ORIG_FLOW.
        ENDIF.

        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
                                                            "AC310000
        "GET VAT FACTOR
        CLEAR: LS_CAPEX, VAR_DEBITVAT.
        READ TABLE LT_CAPEX_FLG WITH KEY
        ASSET       = ORIG_ASSET
        ASSET_CLASS = ORIG_ASSET_CLASS
        AUDITTRAIL  = ORIG_AUDITTRAIL
        CATEGORY    = ORIG_CATEGORY
        ENTITY      = ORIG_ENTITY
        FLOW        = 'CP004'
        RPTCURRENCY = ORIG_CURRENCY
        TIME        = YEAR_INP
        INTO LS_CAPEX.
        IF SY-SUBRC = 0.
          <LS_SIGNEDDATA> = LS_CAPEX-SIGNEDDATA * VAR_ORIGAMOUNT.
          VAR_DEBITVAT = LS_CAPEX-SIGNEDDATA * VAR_ORIGAMOUNT.
          <LS_ASSET_CLASS> = 'AC310000'.
          CLEAR LS_ASSET_CLASS.
          READ TABLE LT_ASSET_CLASS WITH KEY
          ID = <LS_ASSET_CLASS>
          INTO LS_ASSET_CLASS
          BINARY SEARCH.
          IF SY-SUBRC = 0.
            AC_FLOW         = LS_ASSET_CLASS-FLOW.
            <LS_FLOW> = AC_FLOW.
          ELSE.
            <LS_FLOW> = ORIG_FLOW.
          ENDIF.

          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

        ENDIF.
                                                            "AC300000
        <LS_SIGNEDDATA> = (  VAR_ORIGAMOUNT ) * -1.
        "<LS_ASSET_CLASS> = 'AC300000'.
        <LS_ASSET_CLASS> = AC_TRADE_CRED . "'AC300000'.
        CLEAR LS_ASSET_CLASS.
        READ TABLE LT_ASSET_CLASS WITH KEY
        ID = <LS_ASSET_CLASS>
        INTO LS_ASSET_CLASS
        BINARY SEARCH.
        IF SY-SUBRC = 0.
          AC_FLOW         = LS_ASSET_CLASS-FLOW.
          <LS_FLOW> = AC_FLOW.
        ELSE.
          <LS_FLOW> = ORIG_FLOW.
        ENDIF.

        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

        <LS_SIGNEDDATA> = ( VAR_DEBITVAT  ) * -1.
        "<LS_ASSET_CLASS> = 'AC300000'.
        <LS_ASSET_CLASS> = AC_TRADE_CRED . "'AC300000'.
        IF LS_ASSET_CLASS-FLOW_VAT IS INITIAL.
          "DO NOTHING BECAUSE FLOW WILL BE BLANK
        ELSE.
          <LS_FLOW> = LS_ASSET_CLASS-FLOW_VAT.
          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
        ENDIF.

        ENDIF. "ORIG_FLOW <> CP006

        "CLEAR DATA FOR THE TIMESCOPE STATACCTS
        "FLOW VALUE CP100 SIGNEDDATA = 1
        CLEAR LS_CAPEX.
        READ TABLE LT_CAPEX_FLG WITH KEY
        ASSET       = ORIG_ASSET
        ASSET_CLASS = ORIG_ASSET_CLASS
        AUDITTRAIL  = ORIG_AUDITTRAIL
        CATEGORY    = ORIG_CATEGORY
        ENTITY      = ORIG_ENTITY
        FLOW        = 'CP100'
        RPTCURRENCY = ORIG_CURRENCY
        TIME        = YEAR_INP
            INTO LS_CAPEX.
        IF SY-SUBRC = 0 AND LS_CAPEX-SIGNEDDATA = 1. "'CP100'=1
          CLEAR: LS_TIME , LS_ASSET_CLASS.
          "CLEAR DATA IN REGION OF STATACCT START

          "GET CP002 AS A POSTING TIME ID
          READ TABLE LT_CAPEX_FLG WITH KEY
          ASSET       = ORIG_ASSET
          ASSET_CLASS = ORIG_ASSET_CLASS
          AUDITTRAIL  = ORIG_AUDITTRAIL
          CATEGORY    = ORIG_CATEGORY
          ENTITY      = ORIG_ENTITY
          FLOW        = 'CP002'
          RPTCURRENCY = ORIG_CURRENCY
          TIME        = YEAR_INP
          INTO LS_CAPEX.
          IF SY-SUBRC = 0 AND LS_CAPEX-SIGNEDDATA IS NOT INITIAL.
            TIME_STRING = LS_CAPEX-SIGNEDDATA.
            "POST TO ASSET_TYPE TO CP002 TIME ID
            <LS_TIME> = TIME_STRING(4) && '.' && TIME_STRING+4(2).
            <LS_SIGNEDDATA> = VAR_ORIGAMOUNT * -1.
            <LS_ASSET_CLASS> = 'AC219050'.
            CLEAR LS_ASSET_CLASS.
            READ TABLE LT_ASSET_CLASS WITH KEY
            ID = <LS_ASSET_CLASS>
            INTO LS_ASSET_CLASS
            BINARY SEARCH.
            IF SY-SUBRC = 0.
              AC_FLOW         = LS_ASSET_CLASS-FLOW.
              <LS_FLOW> = AC_FLOW.
            ELSE.
              <LS_FLOW> = ORIG_FLOW.
            ENDIF.

            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

            "value of signeddata should be in yyyymm i.e 201710.00 for 2017.10
            <LS_TIME> = TIME_STRING(4) && '.' && TIME_STRING+4(2).
            <LS_SIGNEDDATA> = VAR_ORIGAMOUNT.
            "<LS_ASSET_CLASS> = LS_ASSET_CLASS-BS_COSTACC.
            <LS_ASSET_CLASS> = AC_BS_COSTACC.
            CLEAR LS_ASSET_CLASS.
            READ TABLE LT_ASSET_CLASS WITH KEY
            ID = <LS_ASSET_CLASS>
            INTO LS_ASSET_CLASS
            BINARY SEARCH.
            IF SY-SUBRC = 0.
              AC_FLOW         = LS_ASSET_CLASS-FLOW.
              <LS_FLOW> = AC_FLOW.
            ELSE.
              <LS_FLOW> = ORIG_FLOW.
            ENDIF.

            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

            "CLEARING THE INTERSECTION OF STATACCTS
            LOOP AT LT_CAPEX_STAT INTO LS_CAPEX
              WHERE ASSET = ORIG_ASSET
              AND AUDITTRAIL = ORIG_AUDITTRAIL
              AND CATEGORY = ORIG_CATEGORY
              AND ENTITY = ORIG_ENTITY
*CMC001 Change Delete ****************************
*              AND RPTCURRENCY = ORIG_CURRENCY
*              AND TIME = <LS_TIME>. "CP002 VALUE
*CMC001 Change Delete ****************************
*CMC001 Change Insert ****************************
              AND RPTCURRENCY = ORIG_CURRENCY.
              <LS_TIME> = LS_CAPEX-TIME.
*CMC001 Change Insert ****************************

              <LS_FLOW> = LS_CAPEX-FLOW.
              <LS_ASSET_CLASS> = LS_CAPEX-ASSET_CLASS.
              <LS_SIGNEDDATA> = 0.

              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            ENDLOOP.

          ENDIF.
        ENDIF."'CP100'=1


*************************************************
*CMC001 Change Block Start **********************
*Special clause for Internal Assets

      ELSE.

        <LS_SIGNEDDATA> = VAR_ORIGAMOUNT.
        <LS_ASSET_CLASS> = AC_BS_COSTACC.

        CLEAR LS_ASSET_CLASS.

        READ TABLE LT_ASSET_CLASS WITH KEY
        ID = <LS_ASSET_CLASS>
        INTO LS_ASSET_CLASS
        BINARY SEARCH.
        IF SY-SUBRC = 0 AND ORIG_FLOW <> 'CP006'  .
          AC_FLOW = LS_ASSET_CLASS-FLOW.
          <LS_FLOW> = AC_FLOW.
          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
          <LS_ASSET_CLASS> = 'AC882000'.
          <LS_FLOW> = 'NO_ANALYSIS'.
          <LS_SIGNEDDATA> = <LS_SIGNEDDATA> * -1.
          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
        ELSE.
*          <LS_FLOW> = ORIG_FLOW.
*          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
        ENDIF.

        LOOP AT LT_CAPEX_STAT INTO LS_CAPEX

          WHERE ASSET = ORIG_ASSET
          AND AUDITTRAIL = ORIG_AUDITTRAIL
          AND CATEGORY = ORIG_CATEGORY
          AND ENTITY = ORIG_ENTITY
          AND RPTCURRENCY = ORIG_CURRENCY.

          <LS_TIME> = LS_CAPEX-TIME.
          <LS_FLOW> = LS_CAPEX-FLOW.
          <LS_ASSET_CLASS> = LS_CAPEX-ASSET_CLASS.
          <LS_SIGNEDDATA> = 0.

          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
        ENDLOOP.

      ENDIF.

*CMC001 Change Block End   **********************
*************************************************

      "SUB 3 CAPITALISATION  -- END

********************************************************************************************************************

      "SUB 4 CALC DEPRECIATION  START
      "FLAG CP101 = 1
      "BUDGET
      "CALCULATE DEPRECIATION
      CLEAR LS_CAPEX.
      READ TABLE LT_CAPEX_FLG WITH KEY
      ASSET       = ORIG_ASSET
      ASSET_CLASS = ORIG_ASSET_CLASS
      AUDITTRAIL  = ORIG_AUDITTRAIL
      CATEGORY    = ORIG_CATEGORY
      ENTITY      = ORIG_ENTITY
      FLOW        = 'CP101'
      RPTCURRENCY = ORIG_CURRENCY
      TIME        = YEAR_INP
      INTO LS_CAPEX.
      IF SY-SUBRC = 0 AND LS_CAPEX-SIGNEDDATA = 1.
        CLEAR LS_CAPEX.
        READ TABLE LT_CAPEX_FLG WITH KEY
        ASSET       = ORIG_ASSET
        ASSET_CLASS = ORIG_ASSET_CLASS
        AUDITTRAIL  = ORIG_AUDITTRAIL
        CATEGORY    = ORIG_CATEGORY
        ENTITY      = ORIG_ENTITY
        FLOW        = 'CP001'
        RPTCURRENCY = ORIG_CURRENCY
        TIME        = YEAR_INP
        INTO LS_CAPEX.
        IF SY-SUBRC = 0 AND LS_CAPEX-SIGNEDDATA <> 0.
          VAR_CP001 = LS_CAPEX-SIGNEDDATA.
          VAR_QUOTIENT = VAR_ORIGAMOUNT / VAR_CP001.
          "READ CP003
          CLEAR LS_CAPEX.
          READ TABLE LT_CAPEX_FLG WITH KEY
          ASSET       = ORIG_ASSET
          ASSET_CLASS = ORIG_ASSET_CLASS
          AUDITTRAIL  = ORIG_AUDITTRAIL
          CATEGORY    = ORIG_CATEGORY
          ENTITY      = ORIG_ENTITY
          FLOW        = 'CP003'
          RPTCURRENCY = ORIG_CURRENCY
          TIME        = YEAR_INP
          INTO LS_CAPEX.
          IF SY-SUBRC = 0 AND LS_CAPEX-SIGNEDDATA IS NOT INITIAL.
            "POST QUOTIENT FROM CP003 DATE UP TO CP001 WITHIN TIMESCOPE
            TIME_STRING = LS_CAPEX-SIGNEDDATA."FROM CP003 YYYYMM
            TIME_CP003 = LS_CAPEX-SIGNEDDATA.
            TIME_CP003 = TIME_CP003 && '00'.
            CONDENSE TIME_CP003 NO-GAPS.
            TIME_CP001 = LS_CAPEX-SIGNEDDATA.
            ITIMEID = LS_CAPEX-SIGNEDDATA.
            CLEAR LS_TIME.
            VAR_CP001 = VAR_CP001 - 1.
            DO VAR_CP001 TIMES.
              IF TIME_CP001+4(2) = 12.
                ITIMEID = ITIMEID + 89 .
              ELSE.
                ITIMEID = ITIMEID + 1 .
              ENDIF.
              TIME_CP001 = ITIMEID.
            ENDDO.
            TIME_CP001 = ITIMEID && '00'.
            " TIME_STRING = TIME_STRING && '00'.
            "GET ASSET_CLASS THAT THE DATA WILL BE POSTED ON
            CLEAR LS_ASSET_CLASS.
            READ TABLE LT_ASSET_CLASS WITH KEY
            ID = ORIG_ASSET_CLASS
            INTO LS_ASSET_CLASS.
            IF SY-SUBRC = 0 AND LS_ASSET_CLASS-PL_GLACC IS NOT INITIAL
              AND LS_ASSET_CLASS-BS_DEPAMORT IS NOT INITIAL .

              AC_BS_DEPAMORT  = LS_ASSET_CLASS-BS_DEPAMORT.
              AC_PL_GLACC   = LS_ASSET_CLASS-PL_GLACC.

              "CHECK CATEGORY TO SEE HOW WE POST THE DEPRECIATION
              CLEAR LS_TIME.
              CASE <LS_CATEGORY> .

                WHEN 'BUDGET' .
                  "POST TO DB
                  "TIME_CP001 WILL BE CP001 EXPRESSED IN YYYYMM00
                  LOOP AT LT_TIMESCOPE INTO LS_TIME WHERE
                    TIMEID >= TIME_CP003 AND
                    TIMEID <= TIME_CP001.

                    <LS_ASSET_CLASS> = AC_PL_GLACC.
                    <LS_TIME> = LS_TIME-ID.
                    <LS_SIGNEDDATA> = VAR_QUOTIENT .
                    CLEAR LS_ASSET_CLASS.
                    READ TABLE LT_ASSET_CLASS WITH KEY
                    ID = <LS_ASSET_CLASS>
                    INTO LS_ASSET_CLASS
                    BINARY SEARCH.
                    IF SY-SUBRC = 0.
                      AC_FLOW         = LS_ASSET_CLASS-FLOW.
                      <LS_FLOW> = AC_FLOW.
                    ELSE.
                      <LS_FLOW> = ORIG_FLOW.
                    ENDIF.
                    COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

                    <LS_ASSET_CLASS> = AC_BS_DEPAMORT.
                    <LS_TIME> = LS_TIME-ID.
                    <LS_SIGNEDDATA> = VAR_QUOTIENT * -1  .
                    CLEAR LS_ASSET_CLASS.
                    READ TABLE LT_ASSET_CLASS WITH KEY
                    ID = <LS_ASSET_CLASS>
                    INTO LS_ASSET_CLASS
                    BINARY SEARCH.
                    IF SY-SUBRC = 0.
                      AC_FLOW         = LS_ASSET_CLASS-FLOW.
                      <LS_FLOW> = AC_FLOW.
                    ELSE.
                      <LS_FLOW> = ORIG_FLOW.
                    ENDIF.
                    COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

                  ENDLOOP.

                WHEN 'WFORECAST' .
                  "FLAG CP101 = 1
                  "WFORECAST TIMESCOPE+12 MUST BE USED
                  "CALCULATE DEPRECIATION
                  "TIME_STRING WILL BE CP001 EXPRESSED IN YYYYMM00
                  "CLEAR DATA WITH THE EXTENDED TIMESCOPE

                  "CLEARING THE INTERSECTION OF STATACCTS

                  LOOP AT LT_CAPEX_STAT INTO LS_CAPEX
                    WHERE ASSET = ORIG_ASSET
                    AND AUDITTRAIL = ORIG_AUDITTRAIL
                    AND CATEGORY = ORIG_CATEGORY
                    AND ENTITY = ORIG_ENTITY
                    AND RPTCURRENCY = ORIG_CURRENCY
                    AND TIME IN LT_TIME_RANGE. "THIS MUST BE THE TIMESCOPE PLUS REGION

                    <LS_TIME> = LS_CAPEX-TIME.
                    <LS_FLOW> = LS_CAPEX-FLOW.
                    <LS_ASSET_CLASS> = LS_CAPEX-ASSET_CLASS.
                    <LS_SIGNEDDATA> = 0.
                    COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
                  ENDLOOP.

                  "POST DATA ON TIMESCOPE_PLUS
                  LOOP AT LT_TIMESCOPE_PLUS INTO LS_TIME WHERE
                    TIMEID >= TIME_CP003 AND
                    TIMEID <= TIME_CP001.

                    <LS_ASSET_CLASS> = AC_PL_GLACC.
                    <LS_TIME> = LS_TIME-ID.
                    <LS_SIGNEDDATA> = VAR_QUOTIENT .
                    CLEAR LS_ASSET_CLASS.
                    READ TABLE LT_ASSET_CLASS WITH KEY
                    ID = <LS_ASSET_CLASS>
                    INTO LS_ASSET_CLASS
                    BINARY SEARCH.
                    IF SY-SUBRC = 0.
                      AC_FLOW         = LS_ASSET_CLASS-FLOW.
                      <LS_FLOW> = AC_FLOW.
                    ELSE.
                      <LS_FLOW> = ORIG_FLOW.
                    ENDIF.
                    <LS_FLOW> = AC_FLOW.
                    "APPEND <LS_RESULT_REC> TO <LT_FINAL>.
                    COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

                    <LS_ASSET_CLASS> = AC_BS_DEPAMORT.
                    <LS_TIME> = LS_TIME-ID.
                    <LS_SIGNEDDATA> = VAR_QUOTIENT * -1  .
                    CLEAR LS_ASSET_CLASS.
                    READ TABLE LT_ASSET_CLASS WITH KEY
                    ID = <LS_ASSET_CLASS>
                    INTO LS_ASSET_CLASS
                    BINARY SEARCH.
                    IF SY-SUBRC = 0.
                      AC_FLOW         = LS_ASSET_CLASS-FLOW.
                      <LS_FLOW> = AC_FLOW.
                    ELSE.
                      <LS_FLOW> = ORIG_FLOW.
                    ENDIF.
                    <LS_FLOW> = AC_FLOW.
                    "APPEND <LS_RESULT_REC> TO <LT_FINAL>.
                    COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

                  ENDLOOP.

                WHEN OTHERS.
              ENDCASE.

            ELSE.
              CLEAR: ET_MESSAGE, L_LOG.
              L_LOG = LS_ASSET_CLASS-ID && ' HAS INCOMPLETE PROPERTY MAINTAINED'.
              CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).
            ENDIF.

          ENDIF.
        ENDIF.
      ENDIF.
      "SUB 4 CALC DEPRECIATION  END
**
**      ENDIF."FLAG1
    ENDLOOP.

    "CLEARING OF CP001 - CP999 EXCEPT CP005 FOR CP102 = 1

    IF LT_CAPEX_XXX IS INITIAL.
      "DO NOTHING
    ELSE.
      "CLEAR THE FLAGS.
      CLEAR LT_CT_DATA.
      SORT LT_CAPEX_XXX BY
      ASSET ASSET_CLASS AUDITTRAIL ENTITY FLOW CATEGORY TIME RPTCURRENCY.
      DELETE ADJACENT DUPLICATES FROM LT_CAPEX_XXX COMPARING
      ASSET ASSET_CLASS AUDITTRAIL ENTITY FLOW CATEGORY TIME RPTCURRENCY.

      LOOP AT LT_CAPEX_FLG INTO LS_CAPEX.
        CLEAR LS_CAPEX2.
        READ TABLE LT_CAPEX_XXX
        WITH KEY
        ASSET       = LS_CAPEX-ASSET
        ASSET_CLASS = LS_CAPEX-ASSET_CLASS
        AUDITTRAIL  = LS_CAPEX-AUDITTRAIL
        CATEGORY    = LS_CAPEX-CATEGORY
        ENTITY      = LS_CAPEX-ENTITY
        "FLOW        = 'CP102'
        RPTCURRENCY = LS_CAPEX-RPTCURRENCY
        TIME        = YEAR_INP
        INTO LS_CAPEX2.

        IF SY-SUBRC = 0.
          <LS_ASSET>        = LS_CAPEX-ASSET.
          <LS_ASSET_CLASS>  = LS_CAPEX-ASSET_CLASS.
          <LS_FLOW>         = LS_CAPEX-FLOW.
          <LS_AUDITTRAIL>   = LS_CAPEX-AUDITTRAIL.
          <LS_ENTITY>       = LS_CAPEX-ENTITY.
          <LS_CATEGORY>     = LS_CAPEX-CATEGORY.
          <LS_TIME>         = YEAR_INP.
          <LS_RPTCURRENCY>  = LS_CAPEX-RPTCURRENCY.
          <LS_SIGNEDDATA>   = 0.
          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
        ENDIF.

        CLEAR LS_CAPEX2.
        READ TABLE LT_CAPEX_XXX
        WITH KEY
        ASSET       = LS_CAPEX-ASSET
        ASSET_CLASS = LS_CAPEX-ASSET_CLASS
        AUDITTRAIL  = LS_CAPEX-AUDITTRAIL
        CATEGORY    = LS_CAPEX-CATEGORY
        ENTITY      = LS_CAPEX-ENTITY
        "FLOW        = 'CP102'
        RPTCURRENCY = LS_CAPEX-RPTCURRENCY
        TIME        = YEAR2_INP
        INTO LS_CAPEX2.

        IF SY-SUBRC = 0.
          <LS_ASSET>        = LS_CAPEX-ASSET.
          <LS_ASSET_CLASS>  = LS_CAPEX-ASSET_CLASS.
          <LS_FLOW>         = LS_CAPEX-FLOW.
          <LS_AUDITTRAIL>   = LS_CAPEX-AUDITTRAIL.
          <LS_ENTITY>       = LS_CAPEX-ENTITY.
          <LS_CATEGORY>     = LS_CAPEX-CATEGORY.
          <LS_TIME>         = YEAR_INP.
          <LS_RPTCURRENCY>  = LS_CAPEX-RPTCURRENCY.
          <LS_SIGNEDDATA>   = 0.
          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
        ENDIF.


      ENDLOOP.

      "Change CMC004 Start *********************** TAKEN OUT
*    CLEAR CT_DATA.
*    APPEND LINES OF <LT_FINAL> TO CT_DATA.
      "Change CMC004 End   *********************** TAKEN OUT

    ENDIF.

*********
    "CALL FXTRANS HERE TO CONVERT WE CANNOT DO IT ON THE SCRIPT ON SEPERATE CALLS BECAUSE THE CT_DATA BECOMES BIG I.E. 1 TO MANY
    "DATA TO BE TRANSLATED MUST ONLY BE LC

    "Change CMC004 Start ***********************
    CLEAR CT_DATA.
    APPEND LINES OF <LT_FINAL> TO CT_DATA.
    "Change CMC004 End   ***********************

    ME->FXTRANS(
      EXPORTING
        I_APPSET_ID = I_APPSET_ID
        I_APPL_ID   = I_APPL_ID
        IT_CV       = IT_CV
      IMPORTING
        ET_MESSAGE  = ET_MESSAGE
      CHANGING
        CT_DATA     = <LT_FINAL>
        ).
*********

    APPEND LINES OF <LT_FINAL> TO CT_DATA.

    CLEAR LT_CT_DATA.
    MOVE-CORRESPONDING CT_DATA TO LT_CT_DATA.
    SORT LT_CT_DATA BY
    ASSET ASSET_CLASS AUDITTRAIL ENTITY FLOW CATEGORY TIME RPTCURRENCY.
    DELETE ADJACENT DUPLICATES FROM LT_CT_DATA COMPARING
    ASSET ASSET_CLASS AUDITTRAIL ENTITY FLOW CATEGORY TIME RPTCURRENCY.

    CLEAR CT_DATA.
    MOVE-CORRESPONDING LT_CT_DATA TO CT_DATA.


  ENDMETHOD.


  METHOD CAPEX_NEW.
*&---------------------------------------------------------------------*
****************************************************************************
* Program type....:  Customer SO Custom Method
* Author...........: ET
* Date.............: 05.06.2017
* ----------------------------------------------------------------------
* Description......: This method handles all the relevant calculations
*                    within the CAPEX model - new version of method,
*                    reworked and optimised to handle AUC.
*
*
****************************************************************************
* Change Log:
* - 19/06/2017 Fixing the logic related to the Finance Leases |Defect 3040
****************************************************************************
* Changed on: 19/06/2017              By: ET
* Description:
****************************************************************************


"Common variables
 INCLUDE ZINC_PLAN_BADI_COMMON_VARS.

""""" Local Variables
 DATA: CV_ASSET TYPE UJK_S_CV,
       CV_ASSET_CLASS TYPE UJK_S_CV,
       ASSET_CLASSES TYPE UJA_T_DIM_MEMBER,
       ASSET_CLASSES_FL TYPE UJA_T_DIM_MEMBER,
       ASSETS_EXTERNAL TYPE UJA_T_DIM_MEMBER,
       CATEGORY_YEAR TYPE CHAR4,
       ORIG_TIME TYPE UJ_DIM_MEMBER,
       ORIG_FLOW TYPE UJ_DIM_MEMBER,
       ORIG_SIGNEDDATA TYPE UJ_SDATA,
       IS_EXT_ASSET TYPE CHAR1,
       IS_FIN_LEASE TYPE CHAR1,
       LAST_PERIOD_STR TYPE STRING.


""""Locale Types and Tables
DATA: BEGIN OF LS_CAPEX_DRIVERS,
         ASSET           TYPE UJ_DIM_MEMBER,
         ASSET_CLASS     TYPE UJ_DIM_MEMBER,
         AUDITTRAIL      TYPE UJ_DIM_MEMBER,
         ENTITY          TYPE UJ_DIM_MEMBER,
         FLOW            TYPE UJ_DIM_MEMBER,
         SIGNEDDATA TYPE UJ_SDATA,
       END OF LS_CAPEX_DRIVERS.

DATA: BEGIN OF LS_CAP_DRIVER,
        PERD_DEPR_CP001 TYPE INTEGER,
        CAP_MONTH_CP002 TYPE UJ_SDATA,
        DEP_MONTH_CP003 TYPE UJ_SDATA,
        PURCH_TAX_CP004 TYPE UJ_SDATA,
        ASST_CRTD_CP102 TYPE UJ_SDATA,
      END OF LS_CAP_DRIVER.

DATA: BEGIN OF LS_CATEGORY,
       ID TYPE UJ_DIM_MEMBER,
       YEAR TYPE CHAR32,
      END OF LS_CATEGORY.

DATA: BEGIN OF LS_ASSET_CLASS,
       ID          TYPE UJ_DIM_MEMBER,
       BS_COSTACC  TYPE CHAR32,
       BS_DEPAMORT TYPE CHAR32,
       PL_GLACC    TYPE CHAR32,
       FLOW        TYPE CHAR32,
       FLOW_CRED   TYPE CHAR32,
       FLOW_DEPR   TYPE CHAR32,
       FLOW_VAT    TYPE CHAR32,
       TRADE_CRED  TYPE CHAR32,
       IS_AUC      TYPE CHAR1,
     END OF LS_ASSET_CLASS.

DATA: LT_CAPEX_DRIVERS LIKE TABLE OF LS_CAPEX_DRIVERS,
      LT_ASSET_CLASS LIKE TABLE OF LS_ASSET_CLASS WITH KEY ID,
      LT_CATEGORY LIKE TABLE OF LS_CATEGORY WITH KEY ID,
      LT_TIMESCOPE_EXT LIKE TABLE OF LS_TIME WITH KEY ID.


"Field Symbold
FIELD-SYMBOLS: <LS_ASSET>       TYPE ANY.


********** SET TOOLS INSTANCE *****
    CREATE OBJECT LO_TOOLS
      EXPORTING
        I_ENV_ID = I_APPSET_ID
        I_MOD_ID = I_APPL_ID.

************************ASSIGN STRUCTURE OF INCOMING MODEL ****
    LO_TOOLS->WRITE_MODEL_DATA(
      EXPORTING
       TARGET_MODEL = I_APPL_ID "self structure
        WRITE       = 'N'
     IMPORTING
       LR_DATA     = LR_DATA ).
    ASSIGN LR_DATA->* TO <CT_DATA>.

************ ASSIGN WAs **********************************************
    CREATE DATA LT_FINAL LIKE CT_DATA.
    ASSIGN LT_FINAL->* TO <LT_FINAL>.
    CREATE DATA LS_RESULT_REC LIKE LINE OF CT_DATA.
    ASSIGN LS_RESULT_REC->* TO <LS_RESULT_REC>.
    CREATE DATA LS_REC LIKE LINE OF CT_DATA.
    ASSIGN LS_REC->* TO <LS_REC>.


*************Read all Asset Classes**************************************

      ASSET_CLASSES = LO_TOOLS->GET_PARENT_MD(
      EXPORTING
      PARENT_MBR = 'TOTAL_ASSETCLASS'
      I_DIMENSION_ID = 'ASSET_CLASS').

*************Read Finance Lease Classes**************************************

      ASSET_CLASSES_FL = LO_TOOLS->GET_PARENT_MD(
      EXPORTING
      PARENT_MBR = 'TOTAL_FINLEASE'
      I_DIMENSION_ID = 'ASSET_CLASS').



***********Read All External Assets888888888888888888888888888888888888
      ASSETS_EXTERNAL = LO_TOOLS->GET_PARENT_MD(
      EXPORTING
      PARENT_MBR = 'TOTAL_ASSETS_EXT'
      I_DIMENSION_ID = 'ASSET').

"""""""""""""Read IT_CV Data*****************************

      READ TABLE IT_CV INTO CV_ENTITY WITH TABLE KEY DIM_UPPER_CASE = 'ENTITY'.
      READ TABLE IT_CV INTO CV_ASSET WITH TABLE KEY DIM_UPPER_CASE = 'ASSET'.
      READ TABLE IT_CV INTO CV_AUDITTRAIL WITH TABLE KEY DIM_UPPER_CASE = 'AUDITTRAIL'.


*************************Read CaTEGORY Year*******************************************************
      CLEAR: LT_SEL,LS_SEL.
      LS_SEL-ATTRIBUTE = 'ID'.
      LS_SEL-SIGN = 'I'.
      LS_SEL-OPTION = 'EQ'.
      LS_SEL-DIMENSION = 'CATEGORY'.
      LS_SEL-LOW = WORKING_CATEGORY.
      APPEND LS_SEL TO LT_SEL.


    LO_TOOLS->READ_MASTER_DATA(
      EXPORTING
         I_DIMENSION_ID   = LS_SEL-DIMENSION
      IMPORTING
        OUTPUT_R_DATA = MASTER_DATA
      CHANGING
        LT_SEL        = LT_SEL ).

    ASSIGN MASTER_DATA->* TO <DIMENSION_DATA>.
    MOVE-CORRESPONDING <DIMENSION_DATA> TO LT_CATEGORY.

    READ TABLE LT_CATEGORY INTO LS_CATEGORY WITH TABLE KEY ID = WORKING_CATEGORY.
    CATEGORY_YEAR = LS_CATEGORY-YEAR.

*******TIMES Scope***************************************
      LO_TOOLS->TIME(
          EXPORTING
            CATEGORY    = WORKING_CATEGORY
          IMPORTING
           TIMESCOPE   = LT_TIMESCOPE
           FIRST_PERIOD   = FIRST_PERIOD
           CURRENT_PERIOD = CURRENT_PERIOD
           LAST_PERIOD = LAST_PERIOD ).

****************Read Extended Timescope*********************************
    CLEAR: LT_SEL,LS_SEL.
    IF WORKING_CATEGORY = 'WFORECAST'.
      LS_SEL-DIMENSION = 'TIME'.
      LS_SEL-ATTRIBUTE = 'CURRMONTH'.
      LS_SEL-SIGN = 'I'.
      LS_SEL-OPTION = 'EQ'.
      LS_SEL-LOW = 'A'.
      APPEND LS_SEL TO LT_SEL.
      LS_SEL-ATTRIBUTE = 'YEAR'.
      LS_SEL-LOW = CATEGORY_YEAR.
      APPEND LS_SEL TO LT_SEL.

        LO_TOOLS->READ_MASTER_DATA(
        EXPORTING
           I_DIMENSION_ID   = LS_SEL-DIMENSION
        IMPORTING
          OUTPUT_R_DATA = MASTER_DATA
        CHANGING
          LT_SEL        = LT_SEL ).

      ASSIGN MASTER_DATA->* TO <DIMENSION_DATA>.
      MOVE-CORRESPONDING <DIMENSION_DATA> TO LT_TIMESCOPE_EXT.
    ENDIF.

    APPEND LINES OF LT_TIMESCOPE TO LT_TIMESCOPE_EXT.

***********LS Sel for Asset Classes**************************************

    "ENTITY IT_CV
    CLEAR: LT_SEL,LS_SEL.
    "Commont attributes for LT_SEL
    LS_SEL-ATTRIBUTE = 'ID'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    "END Commont attributes for LT_SEL

     LS_SEL-DIMENSION = 'ASSET_CLASS'.

    LOOP AT ASSET_CLASSES INTO WA_MEMBER.
      LS_SEL-LOW = WA_MEMBER.
      APPEND LS_SEL TO LT_SEL.
    ENDLOOP.


************REad MD Properties for ASSET Classes**************************
    LO_TOOLS->READ_MASTER_DATA(
      EXPORTING
         I_DIMENSION_ID   = LS_SEL-DIMENSION
      IMPORTING
        OUTPUT_R_DATA = MASTER_DATA
      CHANGING
        LT_SEL        = LT_SEL ).

    ASSIGN MASTER_DATA->* TO <DIMENSION_DATA>.
    MOVE-CORRESPONDING <DIMENSION_DATA> TO LT_ASSET_CLASS.


*****Preapre LT_SEL for Reading Drivers Data************************
    "Drivers TIME
    LS_SEL-DIMENSION = 'TIME'.
    LS_SEL-LOW = 'XXXX.INP'.
    APPEND LS_SEL TO LT_SEL.

    "CURRENCY LC
    LS_SEL-DIMENSION = 'RPTCURRENCY'.
    LS_SEL-LOW = 'LC'.
    APPEND LS_SEL TO LT_SEL.


    "ENTITY IT_CV
    LS_SEL-DIMENSION = 'ENTITY'.
    LOOP AT CV_ENTITY-MEMBER INTO WA_MEMBER.
      LS_SEL-LOW = WA_MEMBER.
      APPEND LS_SEL TO LT_SEL.
    ENDLOOP.

    "Assset IT_CV
    LS_SEL-DIMENSION = 'ASSET'.
    LOOP AT CV_ASSET-MEMBER INTO WA_MEMBER.
      LS_SEL-LOW = WA_MEMBER.
      APPEND LS_SEL TO LT_SEL.
    ENDLOOP.

    "Audittrail
    LS_SEL-DIMENSION = 'AUDITTRAIL'.
    LOOP AT CV_AUDITTRAIL-MEMBER INTO WA_MEMBER.
      LS_SEL-LOW = WA_MEMBER.
      APPEND LS_SEL TO LT_SEL.
    ENDLOOP.


    "CATEGORY
      LS_SEL-DIMENSION = 'CATEGORY'.
      LS_SEL-LOW = WORKING_CATEGORY.
      APPEND LS_SEL TO LT_SEL.



*********Read Transactional Data Drivers**********************
        LO_TOOLS->READ_MODEL_DATA(
           EXPORTING
             IT_SEL      = LT_SEL
           IMPORTING
             OUTPUT_DATA = <CT_DATA> ).

        MOVE-CORRESPONDING <CT_DATA> TO LT_CAPEX_DRIVERS.


***************Preapare LT_SEL for Reading Spending Transdactional Data
      "delete xxxx.inp
      DELETE LT_SEL WHERE DIMENSION = 'TIME'.

      "add spendung flows
      LS_SEL-DIMENSION = 'FLOW'.
      LS_SEL-LOW = 'CP006'.
      APPEND LS_SEL TO LT_SEL.
      LS_SEL-LOW = 'CP005'.
      APPEND LS_SEL TO LT_SEL.

      "Add time Extended Scope + Category Year.INP
      LS_SEL-DIMENSION = 'TIME'.
      LS_SEL-LOW = CATEGORY_YEAR && '.INP'.
      APPEND LS_SEL TO LT_SEL.

      LOOP AT LT_TIMESCOPE_EXT INTO LS_TIME.
        LS_SEL-LOW = LS_TIME-ID.
        APPEND LS_SEL TO LT_SEL.
      ENDLOOP.

*********Read Spending data for the Asset********************
        LO_TOOLS->READ_MODEL_DATA(
           EXPORTING
             IT_SEL      = LT_SEL
           IMPORTING
             OUTPUT_DATA = <CT_DATA> ).

        CLEAR CT_DATA.
        MOVE-CORRESPONDING <CT_DATA> TO CT_DATA.

*************************************************************



LOOP AT CT_DATA ASSIGNING <LS_REC>.
      <LS_RESULT_REC> = <LS_REC>.
      ASSIGN COMPONENT 'ASSET'        OF STRUCTURE <LS_RESULT_REC> TO <LS_ASSET>.
      ASSIGN COMPONENT 'ASSET_CLASS'  OF STRUCTURE <LS_RESULT_REC> TO <LS_ASSET_CLASS>.
      ASSIGN COMPONENT 'AUDITTRAIL'   OF STRUCTURE <LS_RESULT_REC> TO <LS_AUDITTRAIL>.
      ASSIGN COMPONENT 'ENTITY'       OF STRUCTURE <LS_RESULT_REC> TO <LS_ENTITY>.
      ASSIGN COMPONENT 'FLOW'         OF STRUCTURE <LS_RESULT_REC> TO <LS_FLOW>.
      ASSIGN COMPONENT 'TIME'         OF STRUCTURE <LS_RESULT_REC> TO <LS_TIME>.
      ASSIGN COMPONENT 'SIGNEDDATA'   OF STRUCTURE <LS_RESULT_REC> TO <LS_SIGNEDDATA>.

            "Initial Values
            ORIG_TIME = <LS_TIME>.
            ORIG_FLOW = <LS_FLOW>.
            ORIG_SIGNEDDATA = <LS_SIGNEDDATA>.

************Read Drivers from the Input (Transaction Type Data)




            "Read Periods of Depreciation
            CLEAR LS_CAPEX_DRIVERS.
            READ TABLE LT_CAPEX_DRIVERS INTO LS_CAPEX_DRIVERS WITH TABLE KEY
                                                              ASSET = <LS_ASSET>
                                                              ASSET_CLASS = <LS_ASSET_CLASS>
                                                              AUDITTRAIL = <LS_AUDITTRAIL>
                                                              ENTITY = <LS_ENTITY>
                                                              FLOW = 'CP001'.

            LS_CAP_DRIVER-PERD_DEPR_CP001 = LS_CAPEX_DRIVERS-SIGNEDDATA.


            "Read Capitalisation Month
            CLEAR LS_CAPEX_DRIVERS.
            READ TABLE LT_CAPEX_DRIVERS INTO LS_CAPEX_DRIVERS WITH TABLE KEY
                                                              ASSET = <LS_ASSET>
                                                              ASSET_CLASS = <LS_ASSET_CLASS>
                                                              AUDITTRAIL = <LS_AUDITTRAIL>
                                                              ENTITY = <LS_ENTITY>
                                                              FLOW = 'CP002'.

            LS_CAP_DRIVER-CAP_MONTH_CP002 = LS_CAPEX_DRIVERS-SIGNEDDATA.


            "Read Depreciation Month
            CLEAR LS_CAPEX_DRIVERS.
            READ TABLE LT_CAPEX_DRIVERS INTO LS_CAPEX_DRIVERS WITH TABLE KEY
                                                              ASSET = <LS_ASSET>
                                                              ASSET_CLASS = <LS_ASSET_CLASS>
                                                              AUDITTRAIL = <LS_AUDITTRAIL>
                                                              ENTITY = <LS_ENTITY>
                                                              FLOW = 'CP003'.
            LS_CAP_DRIVER-DEP_MONTH_CP003 = LS_CAPEX_DRIVERS-SIGNEDDATA.


            "Read Purchase Tax
            CLEAR LS_CAPEX_DRIVERS.
            READ TABLE LT_CAPEX_DRIVERS INTO LS_CAPEX_DRIVERS WITH TABLE KEY
                                                              ASSET = <LS_ASSET>
                                                              ASSET_CLASS = <LS_ASSET_CLASS>
                                                              AUDITTRAIL = <LS_AUDITTRAIL>
                                                              ENTITY = <LS_ENTITY>
                                                              FLOW = 'CP004'.
            LS_CAP_DRIVER-PURCH_TAX_CP004 = LS_CAPEX_DRIVERS-SIGNEDDATA.

            "Read Asset Created Flag
            CLEAR LS_CAPEX_DRIVERS.
            READ TABLE LT_CAPEX_DRIVERS INTO LS_CAPEX_DRIVERS WITH TABLE KEY
                                                              ASSET = <LS_ASSET>
                                                              ASSET_CLASS = <LS_ASSET_CLASS>
                                                              AUDITTRAIL = <LS_AUDITTRAIL>
                                                              ENTITY = <LS_ENTITY>
                                                              FLOW = 'CP102'.
            LS_CAP_DRIVER-ASST_CRTD_CP102 = LS_CAPEX_DRIVERS-SIGNEDDATA.

***********Read Master Data properties

            READ TABLE LT_ASSET_CLASS INTO LS_ASSET_CLASS WITH TABLE KEY ID = <LS_ASSET_CLASS>.


***********Check Whether the asset is external or internal**********************
            READ TABLE ASSETS_EXTERNAL WITH TABLE KEY TABLE_LINE = <LS_ASSET> TRANSPORTING NO FIELDS.
            IF SY-SUBRC = 0.
               IS_EXT_ASSET = 'Y'.
            ELSE.
               IS_EXT_ASSET = 'N'.
            ENDIF.


***********Check Whether the asset is Finance Lease or not*********************
            READ TABLE ASSET_CLASSES_FL WITH TABLE KEY TABLE_LINE = <LS_ASSET_CLASS> TRANSPORTING NO FIELDS.
            IF SY-SUBRC = 0.
               IS_FIN_LEASE = 'Y'.
            ELSE.
               IS_FIN_LEASE = 'N'.
            ENDIF.


********If Asset CREATED is 1 Delete Everyting**********************IF Asset created is not 1 than it follows the main logic below.

          IF LS_CAP_DRIVER-ASST_CRTD_CP102 = 1.


            "Clear Everything beneath
            <LS_SIGNEDDATA> = 0.

            "Clearing Drivers
            <LS_TIME> = 'XXXX.INP'.

            <LS_FLOW> = 'CP001'.
             COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

            <LS_FLOW> = 'CP002'.
             COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

            <LS_FLOW> = 'CP003'.
             COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

            <LS_FLOW> = 'CP004'.
             COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

            <LS_FLOW> = 'CP102'.
             COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.


           "CLEARING OF CP005
            LOOP AT LT_TIMESCOPE_EXT INTO LS_TIME.
              <LS_TIME> = LS_TIME-ID.
              <LS_FLOW> = 'CP005'.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            ENDLOOP.

            "CLEARING OF CP006

              <LS_TIME> = CATEGORY_YEAR && '.INP'.
              <LS_FLOW> = 'CP006'.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.


            "Clearing BS Entries when spending
            IF ORIG_FLOW = 'CP005'.
              "Setting the Initial Time
              <LS_TIME> = ORIG_TIME.

               "Additions
              <LS_ASSET_CLASS> = LS_ASSET_CLASS-BS_COSTACC. "External Assets in the month
              <LS_FLOW> = LS_ASSET_CLASS-FLOW.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

              <LS_ASSET_CLASS> = 'AC219050'. "Other Debtors
              <LS_FLOW> = LS_ASSET_CLASS-FLOW_CRED. "
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

              "Internal Asset PnL
              <LS_ASSET_CLASS> = 'AC882000'. "Internal Asset
              <LS_FLOW> = 'NO_ANALYSIS'.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

              "VAT Records
              IF LS_ASSET_CLASS-FLOW_VAT IS NOT INITIAL.

                "Indirect Tax Acct
                <LS_ASSET_CLASS> = 'AC310000'. "PURCHASETAX
                <LS_FLOW> = LS_ASSET_CLASS-FLOW_VAT.
                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

                "Trade Creditor Acct
                <LS_ASSET_CLASS> = LS_ASSET_CLASS-TRADE_CRED.
                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
              ENDIF.

              "Trade Creditor or Obligation Under FL
                <LS_ASSET_CLASS> = LS_ASSET_CLASS-TRADE_CRED.
                <LS_FLOW> = LS_ASSET_CLASS-FLOW_CRED.
                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            ENDIF. "End ORIG_FLOW = 'CP005'

            "Clearing BS Accumulated Depreciation and PnL Deprecation for all deprecation Periods


            LOOP AT LT_TIMESCOPE_EXT INTO LS_TIME.

              "P&L Depreciation
              <LS_FLOW> = 'NO_ANALYSIS'.
              <LS_ASSET_CLASS> = LS_ASSET_CLASS-PL_GLACC.
              <LS_TIME> = LS_TIME-ID.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

              "BS Accum. Depreciation Movements
              <LS_ASSET_CLASS> = LS_ASSET_CLASS-BS_DEPAMORT.
              <LS_FLOW> = LS_ASSET_CLASS-FLOW_DEPR.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

            ENDLOOP.

            "Clearing CAP Month Entries

            IF LS_CAP_DRIVER-CAP_MONTH_CP002 <> 0.
              LS_TIMEID = LS_CAP_DRIVER-CAP_MONTH_CP002.
              <LS_TIME> = LS_TIMEID(4) && '.' && LS_TIMEID+4(2).
              <LS_ASSET_CLASS> = LS_ASSET_CLASS-BS_COSTACC. "External Assets in the month
              <LS_FLOW> = LS_ASSET_CLASS-FLOW.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
              <LS_ASSET_CLASS> = 'AC219050'. "Other Debtors
              <LS_FLOW> = LS_ASSET_CLASS-FLOW_CRED.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            ENDIF.



         ELSE. "Asset is not created <> 1

*******************Asset is not created <> 1
***********Clear Entries created from CAP Month and Depreciation **********


            "Clear the depreciation and accumulated depreciation first
            <LS_SIGNEDDATA> = 0. "Clearing Depreciation and BS Accum. Depreciation

            LOOP AT LT_TIMESCOPE INTO LS_TIME.

              "P&L Depreciation
              <LS_FLOW> = 'NO_ANALYSIS'.
              <LS_ASSET_CLASS> = LS_ASSET_CLASS-PL_GLACC.
              <LS_TIME> = LS_TIME-ID.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

              "BS Accum. Depreciation Movements
              <LS_ASSET_CLASS> = LS_ASSET_CLASS-BS_DEPAMORT.
              <LS_FLOW> = LS_ASSET_CLASS-FLOW_DEPR.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

              <LS_ASSET_CLASS> = LS_ASSET_CLASS-BS_COSTACC. "Asset Cost Acct
              <LS_FLOW> = LS_ASSET_CLASS-FLOW.
               COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

              "Crediting Other Debtors
              <LS_ASSET_CLASS> = 'AC219050'. "Other Debtors
              <LS_FLOW> = LS_ASSET_CLASS-FLOW_CRED. "
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.


            ENDLOOP.



*********Spending Month****************************

          "Initialise LS_SIGNEDDATA and Time
          <LS_SIGNEDDATA> = ORIG_SIGNEDDATA.
          <LS_TIME> = ORIG_TIME.
           LS_TIMEID = ORIG_TIME(4) && ORIG_TIME+5(2) && '00'.

          "Check if Spending is in a planning month
          IF ORIG_FLOW = 'CP005' AND LS_TIMEID >= FIRST_PERIOD.

              IF IS_EXT_ASSET = 'Y'.
                  IF LS_ASSET_CLASS-IS_AUC <> 'Y' AND IS_FIN_LEASE <> 'Y'.
                    "Other Debtors
                    <LS_ASSET_CLASS> = 'AC219050'. "Other Debtors
                    <LS_FLOW> = LS_ASSET_CLASS-FLOW_CRED. "
                    COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

                  ELSE. "IS_AUC = Y
                    <LS_ASSET_CLASS> = LS_ASSET_CLASS-BS_COSTACC. "Asset Cost Acct
                    <LS_FLOW> = LS_ASSET_CLASS-FLOW.
                    COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

                 ENDIF. "IS Auc <> Y
                    "Entries applicable for both AUC and Non-AUC

                    "Trade Creditors or Obligation Under Finance Lease
                    <LS_ASSET_CLASS> = LS_ASSET_CLASS-TRADE_CRED.
                    <LS_FLOW> = LS_ASSET_CLASS-FLOW_CRED.
                    <LS_SIGNEDDATA> = ORIG_SIGNEDDATA * -1.
                    COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

                    "VAT TAX
                    IF LS_ASSET_CLASS-FLOW_VAT IS NOT INITIAL.

                        "Indirect Tax on the Trade Creditors account
                       <LS_ASSET_CLASS> = LS_ASSET_CLASS-TRADE_CRED.
                       <LS_FLOW> = <LS_FLOW> = LS_ASSET_CLASS-FLOW_VAT.
                       <LS_SIGNEDDATA> = ORIG_SIGNEDDATA * LS_CAP_DRIVER-PURCH_TAX_CP004 * -1.
                        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

                        "Indirect Tax Credit on the liability account
                       <LS_ASSET_CLASS> = 'AC310000'.
                       <LS_FLOW> = <LS_FLOW> = LS_ASSET_CLASS-FLOW_VAT.
                       <LS_SIGNEDDATA> = ORIG_SIGNEDDATA * LS_CAP_DRIVER-PURCH_TAX_CP004.
                        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

                    ENDIF.


              ELSE. "Internal Asset

                    "Check if Spending is in a planning month
                    IF LS_TIMEID >= FIRST_PERIOD.
                     "Asset Cost Account
                     <LS_ASSET_CLASS> = LS_ASSET_CLASS-BS_COSTACC. "Asset Cost Acct
                     <LS_FLOW> = LS_ASSET_CLASS-FLOW.
                     COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

                     "Own Work Capitalised Account
                     <LS_ASSET_CLASS> = 'AC882000'.
                     <LS_FLOW> = 'NO_ANALYSIS'.
                     <LS_SIGNEDDATA> = ORIG_SIGNEDDATA * -1.
                     COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
                   ENDIF.
              ENDIF. "Check External Asset Or Internal Asset

          ENDIF.   "CP005


************Capitalisation Month*****************************************************



            "When CAP is not zero
            IF LS_CAP_DRIVER-CAP_MONTH_CP002 <> 0 AND LS_ASSET_CLASS-IS_AUC IS INITIAL AND IS_FIN_LEASE = 'N'.
              CLEAR LS_TIMEID.
              LS_TIMEID = LS_CAP_DRIVER-CAP_MONTH_CP002.
              <LS_TIME> = LS_TIMEID(4) && '.' && LS_TIMEID+4(2).

              "Debiting Asset Cost Account
              <LS_ASSET_CLASS> = LS_ASSET_CLASS-BS_COSTACC. "Asset Cost Acct
              <LS_FLOW> = LS_ASSET_CLASS-FLOW.
               <LS_SIGNEDDATA> = ORIG_SIGNEDDATA.
               COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

              "Crediting Other Debtors
              <LS_ASSET_CLASS> = 'AC219050'. "Other Debtors
              <LS_FLOW> = LS_ASSET_CLASS-FLOW_CRED. "
              <LS_SIGNEDDATA> = ORIG_SIGNEDDATA * -1.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.


            ENDIF.



*************Depreciation Month

       "If DEPREC_MONTH <> 0 and Periods of depreciation <> 0

          IF LS_CAP_DRIVER-DEP_MONTH_CP003 <> 0 AND LS_CAP_DRIVER-PERD_DEPR_CP001 <> 0.
              "Getting the First Depreciation Month
              LS_TIMEID = LS_CAP_DRIVER-DEP_MONTH_CP003.
              YEAR = LS_TIMEID(4).
              MONTH = LS_TIMEID+4(2).
              <LS_SIGNEDDATA> = ORIG_SIGNEDDATA / LS_CAP_DRIVER-PERD_DEPR_CP001. "Depreciation Monthly Amount

                "Last period of the current Category
                CLEAR LAST_PERIOD_STR.
                LAST_PERIOD_STR = LAST_PERIOD.
                LAST_PERIOD_STR = LAST_PERIOD_STR(4) && '.' && LAST_PERIOD_STR+4(2).

              DO LS_CAP_DRIVER-PERD_DEPR_CP001 TIMES.
                <LS_FLOW> = 'NO_ANALYSIS'.
                <LS_ASSET_CLASS> = LS_ASSET_CLASS-PL_GLACC.
                IF STRLEN( MONTH ) = 1.
                  <LS_TIME> = YEAR && '.' && '0' && MONTH.
                ELSE.
                  <LS_TIME> = YEAR && '.' && MONTH.
                ENDIF.
                <LS_SIGNEDDATA> = <LS_SIGNEDDATA>.
                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

                "BS Accum. Depreciation Movements
                <LS_ASSET_CLASS> = LS_ASSET_CLASS-BS_DEPAMORT.
                <LS_FLOW> = LS_ASSET_CLASS-FLOW_DEPR.
                <LS_SIGNEDDATA> = <LS_SIGNEDDATA> * -1. "Negative value for the accumulated depreciation
                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

                IF <LS_TIME> = LAST_PERIOD_STR.
                  EXIT.
                ENDIF.

                <LS_SIGNEDDATA> = <LS_SIGNEDDATA> * -1. "Make the value Positive
                MONTH = MONTH + 1.
                IF MONTH = 13.
                  YEAR = YEAR + 1.
                  MONTH = 01.
                ENDIF.
              ENDDO.

          ENDIF. "Depreciation Moonth

       ENDIF. "Asset Created = 1 or not created

ENDLOOP.


"Commit the LT_FINAL changes
CT_DATA = <LT_FINAL>.


***********FX Translate Data******************************************************
    ME->FXTRANS(
      EXPORTING
        I_APPSET_ID = I_APPSET_ID
        I_APPL_ID   = I_APPL_ID
        IT_CV       = IT_CV
      IMPORTING
        ET_MESSAGE  = ET_MESSAGE
      CHANGING
        CT_DATA     = <LT_FINAL>
        ).

APPEND LINES OF <LT_FINAL> TO CT_DATA.

ENDMETHOD.


  METHOD CASHFLOW.
*&---------------------------------------------------------------------*
*&  Method           CASHFLOW
*&---------------------------------------------------------------------*
****************************************************************************
* Program type....:  Customer SO Custom Method
* Author...........:
* Date.............: February 2017
* ----------------------------------------------------------------------
* Description......: This method handles CASHFLOW model calculations of
*                    Customer SO_PLANNING BPC model
*
*
****************************************************************************
* Change Log:
* 1. Purchase Tax CR (SUBPX) - ET code.
* 2. Change AC220000 signage.
* 3. Quarterly VAT settlements only for one PC of S101 Company Code
* 4. CR2690 - Capital repayments financial leases
* 5. CR3101 - Intercompany balances settlement
****************************************************************************
* Changed on: 22.08.2017                By: VZ
* Description:
****************************************************************************

    INCLUDE ZINC_PLAN_BADI_COMMON_VARS. "Common Variables

*** SET TOOLS INSTANCE *****
    CREATE OBJECT LO_TOOLS
      EXPORTING
        I_ENV_ID = I_APPSET_ID
        I_MOD_ID = I_APPL_ID
        I_T_CV   = IT_CV.

************************ASSIGN STRUCTURE OF INCOMING MODEL ****
    LO_TOOLS->WRITE_MODEL_DATA(
      EXPORTING
        TARGET_MODEL = I_APPL_ID "self structure
        WRITE       = 'N'
     IMPORTING
       LR_DATA     = LR_DATA ).

    ASSIGN LR_DATA->* TO <CT_DATA>.
**********************************************************************
************ ASSIGN WAs **********************************************
    CREATE DATA LT_FINAL LIKE CT_DATA.
    ASSIGN LT_FINAL->* TO <LT_FINAL>.
    CREATE DATA LS_RESULT_REC LIKE LINE OF CT_DATA.
    ASSIGN LS_RESULT_REC->* TO <LS_RESULT_REC>.
    CREATE DATA LS_REC LIKE LINE OF CT_DATA.
    ASSIGN LS_REC->* TO <LS_REC>.
***************************************************************************

****** GET TIMESCOPE AND VARIABLES

    IF SUB_ID+4(1) = 'A'.

      LO_TOOLS->TIME(
          EXPORTING
            CATEGORY    = WORKING_CATEGORY
            EXTEND = 'A'
          IMPORTING
           TIMEQRTLY   = LT_TIMEQRTLY
           TIMESCOPE   = LT_TIMESCOPE
           FIRST_PERIOD   = FIRST_PERIOD
           LAST_PERIOD = LAST_PERIOD
           CURRENT_PERIOD = CURRENT_PERIOD ).

    ELSE.

      LO_TOOLS->TIME(
        EXPORTING
          CATEGORY    = WORKING_CATEGORY
        IMPORTING
         TIMEQRTLY   = LT_TIMEQRTLY
         TIMESCOPE   = LT_TIMESCOPE
         FIRST_PERIOD   = FIRST_PERIOD
         LAST_PERIOD = LAST_PERIOD
         CURRENT_PERIOD = CURRENT_PERIOD ).

    ENDIF.

******* GET LIST OF RELEVANT FLOWS ********************************

    FLOWS = LO_TOOLS->GET_PARENT_MD(
      EXPORTING
      PARENT_MBR = 'F_CLO_CALC'
      I_DIMENSION_ID = 'FLOW').

****** GET LIST OF RELEVANT ACCOUNTS *****************************

    ACCOUNTS = LO_TOOLS->GET_PARENT_MD(
      EXPORTING
      PARENT_MBR = 'ZM_BS'
      I_DIMENSION_ID = 'ACCOUNT').


****** GET PC and ACCOUNTS from CV ********************************

    READ TABLE IT_CV INTO CV_PROFIT_CENTRE WITH TABLE KEY DIM_UPPER_CASE = 'PROFIT_CENTRE'.
    READ TABLE IT_CV INTO CV_PARTNER WITH TABLE KEY DIM_UPPER_CASE = 'PARTNER'.
    READ TABLE IT_CV INTO CV_ACCOUNTS WITH TABLE KEY DIM_UPPER_CASE = 'ACCOUNT'.


****** END GET TIMESCOPE AND VARIABLES

******************** SUBS START **************************************

    CASE SUB_ID.

*************************** SUB 6 **********************************************
********************************************************************************
      WHEN 'SUB4'. "Settle ICs into F_CASH



      WHEN 'SUBPX'. "Purchase tax modification


        TVARVC_NAME =  ZCL_TVARVC=>GET_SINGLE_VALUE( 'SO_PLANNING-CASHFLOW-SUB3' ).

        ""Reading the Business Rules Table
        SELECT * FROM UJP_CALC_ACCOUNT INTO CORRESPONDING FIELDS OF TABLE LT_TARGET_DATA
          WHERE APPSET_ID = I_APPSET_ID AND
                APPLICATION_ID  = I_APPL_ID AND
                CALC_ID = TVARVC_NAME.


        LOOP AT CV_ACCOUNTS-MEMBER INTO WA_MEMBER.

          "Second check is for the destination table
          LOOP AT LT_TARGET_DATA INTO LS_TARGET_DATA.
            IF LS_TARGET_DATA-ACCOUNT = WA_MEMBER AND LS_TARGET_DATA-DEST_ACCOUNT = 'AC300000'.
              WA_MEMBER = 'AC300000'.
              EXIT.
            ENDIF.
          ENDLOOP.

          "IF WA_MEMBER = 'AC300000'.                           "Old CR 2690 Capital repayments Finance Lease
          IF WA_MEMBER = 'AC300000' OR WA_MEMBER = 'AC320000'.  "New CR 2690 Capital repayments Finance Lease
            EXIT.
          ENDIF.

        ENDLOOP.

        "IF WA_MEMBER = 'AC300000'.                           "Old CR 2690 Capital repayments Finance Lease
        IF WA_MEMBER = 'AC300000' OR WA_MEMBER = 'AC320000'.  "New CR 2690 Capital repayments Finance Lease
        ELSE.
          EXIT.
        ENDIF.

        "Reference Sturcture for INC_STATEMENT Model
        LO_TOOLS->WRITE_MODEL_DATA(
          EXPORTING
            TARGET_MODEL = 'INC_STATEMENT'
            WRITE       = 'N'
         IMPORTING
           LR_DATA     = LR_DATA ).

        ASSIGN LR_DATA->* TO <TARGET_DATA>.

********Selection for INC_STATEMENT Purchase Tax


        LS_SEL-ATTRIBUTE = 'ID'.
        LS_SEL-SIGN = 'I'.
        LS_SEL-OPTION = 'EQ'.

        LOOP AT CV_PROFIT_CENTRE-MEMBER INTO WA_MEMBER.
          LS_SEL-DIMENSION = 'ENTITY'.
          WA_MEMBER = WA_MEMBER && 'BS'.
          LS_SEL-LOW = WA_MEMBER.
          APPEND LS_SEL TO LT_SEL.
        ENDLOOP.

        """"Time Dimension
        LS_SEL-DIMENSION = 'TIME'.
        LS_SEL-LOW = 'XXXX.INP'.
        APPEND LS_SEL TO LT_SEL.

        """"Analysis Dimension
        LS_SEL-DIMENSION = 'ANALYSIS'.
        LS_SEL-LOW = 'NO_ANALYSIS'.
        APPEND LS_SEL TO LT_SEL.


        "" Audittrail Selection
        READ TABLE IT_CV INTO CV_AUDITTRAIL WITH TABLE KEY DIM_UPPER_CASE = 'AUDITTRAIL'.

        CLEAR WA_MEMBER.

        LS_SEL-DIMENSION = 'AUDITTRAIL'.
        LOOP AT CV_AUDITTRAIL-MEMBER INTO WA_MEMBER.
          LS_SEL-LOW = WA_MEMBER. "Can be Input if we want to calculate this for other audittrails
          APPEND LS_SEL TO LT_SEL.
        ENDLOOP.

        ""Category for selection
        LS_SEL-DIMENSION = 'CATEGORY'.
        LS_SEL-LOW = WORKING_CATEGORY.
        APPEND LS_SEL TO LT_SEL.

        ""Partner for selection
        LS_SEL-DIMENSION = 'PARTNER'.
        LS_SEL-LOW = 'I_NONE'.
        APPEND LS_SEL TO LT_SEL.

        ""Indirect Tax Account
        LS_SEL-DIMENSION = 'ACCOUNT'.
        LS_SEL-LOW = 'DR001'.
        APPEND LS_SEL TO LT_SEL.

        ""Indirect Tax Account
        LS_SEL-DIMENSION = 'CURRENCY'.
        LS_SEL-LOW = 'LC'.
        APPEND LS_SEL TO LT_SEL.

************Read Purchase Tax ****************************************

        LO_TOOLS->READ_MODEL_DATA(
           EXPORTING
             READ_MODEL = 'INC_STATEMENT'
             IT_SEL      = LT_SEL
           IMPORTING
             OUTPUT_DATA = <TARGET_DATA> ).

        MOVE-CORRESPONDING <TARGET_DATA> TO LT_DRIVERS.

        "Remove the BS suffix from
        LOOP AT LT_DRIVERS INTO LS_DRIVERS.
          LS_DRIVERS-ENTITY = LS_DRIVERS-ENTITY(12).
          MODIFY LT_DRIVERS FROM LS_DRIVERS.
        ENDLOOP.

****Check if IT_CV or Destination Accounts have Trade Creditors Account AC300000

        ""Deleting the current Accounts
        DELETE LT_SEL WHERE DIMENSION = 'ACCOUNT'.
        DELETE LT_SEL WHERE DIMENSION = 'ENTITY'.
        DELETE LT_SEL WHERE DIMENSION = 'ANALYSIS'.
        DELETE LT_SEL WHERE DIMENSION = 'TIME'.

        CLEAR: WA_MEMBER,LS_TARGET_DATA, LT_TARGET_DATA.



********* READ TARGET ACCOUNTS FROM ACCOUNT TRANSFORMATIONS ******************



        "Set Account
        LS_SEL-DIMENSION = 'ACCOUNT'.
        LS_SEL-LOW = 'AC300000'.
        APPEND LS_SEL TO LT_SEL.
        LS_SEL-DIMENSION = 'ACCOUNT'.             "New CR 2690 Capital repayments Finance Lease
        LS_SEL-LOW = 'AC320000'.                  "New CR 2690 Capital repayments Finance Lease
        APPEND LS_SEL TO LT_SEL.                  "New CR 2690 Capital repayments Finance Lease


        "" Profit Centre
        LS_SEL-DIMENSION = 'PROFIT_CENTRE'.
        LOOP AT CV_PROFIT_CENTRE-MEMBER INTO WA_MEMBER.
          LS_SEL-LOW = WA_MEMBER.
          APPEND LS_SEL TO LT_SEL.
        ENDLOOP.

        "" Time scope
        LS_SEL-DIMENSION = 'TIME'.
        LOOP AT LT_TIMESCOPE INTO LS_TIME.
          LS_SEL-LOW = LS_TIME-ID.
          APPEND LS_SEL TO LT_SEL.
        ENDLOOP.

        ""Flow Dimension Getting from prperty AUTO_PURCH_TAX
        LS_SEL-DIMENSION = 'FLOW'.

        CALL METHOD LO_TOOLS->READ_MASTER_DATA
          EXPORTING
            I_DIMENSION_ID = LS_SEL-DIMENSION
            PROPERTY_ID    = 'AUTO_PURCH_TAX'
            PROPERTY_VALUE = 'Y'
          IMPORTING
            OUTPUT_R_DATA  = MASTER_DATA.

        ASSIGN MASTER_DATA->* TO <DIMENSION_DATA>.
        FLOWS = <DIMENSION_DATA>.


        LOOP AT FLOWS INTO WA_MEMBER.
          LS_SEL-LOW = WA_MEMBER.
          APPEND LS_SEL TO LT_SEL.
        ENDLOOP.


        "" Read Data for AUTO Purchase Tax
        LO_TOOLS->READ_MODEL_DATA(
           EXPORTING
             IT_SEL      = LT_SEL
           IMPORTING
             OUTPUT_DATA = <CT_DATA> ).

        CLEAR CT_DATA.
        MOVE-CORRESPONDING <CT_DATA> TO CT_DATA.

        ""Looping through CT_DATA
        LOOP AT CT_DATA ASSIGNING <LS_REC>.
          ASSIGN COMPONENT 'ACCOUNT' OF STRUCTURE <LS_REC> TO <LS_ACCOUNT>.         "New CR 2690 Capital repayments Finance Lease
          ASSIGN COMPONENT 'FLOW' OF STRUCTURE <LS_REC> TO <LS_FLOW>.
          ASSIGN COMPONENT 'AUDITTRAIL' OF STRUCTURE <LS_REC> TO <LS_AUDITTRAIL>.
          ASSIGN COMPONENT 'PROFIT_CENTRE' OF STRUCTURE <LS_REC> TO <LS_PROFIT_CENTRE>.
          ASSIGN COMPONENT 'SIGNEDDATA' OF STRUCTURE <LS_REC> TO <LS_SIGNEDDATA>.

          READ TABLE LT_DRIVERS INTO LS_DRIVERS WITH KEY
          AUDITTRAIL = <LS_AUDITTRAIL>
          ENTITY = <LS_PROFIT_CENTRE> .

          CASE <LS_ACCOUNT>.                                                        "New CR 2690 Capital repayments Finance Lease
            WHEN 'AC320000'.                                                        "New CR 2690 Capital repayments Finance Lease
              <LS_SIGNEDDATA> = <LS_SIGNEDDATA> * LS_DRIVERS-SIGNEDDATA * -1.       "New CR 2690 Capital repayments Finance Lease
              <LS_FLOW> = 'F_PURCHASETAX_CALC'.                                     "New CR 2690 Capital repayments Finance Lease
              COLLECT <LS_REC> INTO <LT_FINAL>.                                     "New CR 2690 Capital repayments Finance Lease
              <LS_SIGNEDDATA> = <LS_SIGNEDDATA> * -1.                               "New CR 2690 Capital repayments Finance Lease
              <LS_FLOW> = 'F_CASH'.                                                 "New CR 2690 Capital repayments Finance Lease
              COLLECT <LS_REC> INTO <LT_FINAL>.                                     "New CR 2690 Capital repayments Finance Lease
            WHEN 'AC300000'.                                                        "New CR 2690 Capital repayments Finance Lease
              <LS_SIGNEDDATA> = <LS_SIGNEDDATA> * LS_DRIVERS-SIGNEDDATA.            "New CR 2690 Capital repayments Finance Lease
              <LS_FLOW> = 'F_PURCHASETAX_CALC'.                                     "New CR 2690 Capital repayments Finance Lease
              COLLECT <LS_REC> INTO <LT_FINAL>.                                     "New CR 2690 Capital repayments Finance Lease
          ENDCASE.                                                                  "New CR 2690 Capital repayments Finance Lease

          "<LS_SIGNEDDATA> = <LS_SIGNEDDATA> * LS_DRIVERS-SIGNEDDATA.               "Old CR 2690 Capital repayments Finance Lease
          "<LS_FLOW> = 'F_PURCHASETAX_CALC'.                                        "Old CR 2690 Capital repayments Finance Lease

          "COLLECT <LS_REC> INTO <LT_FINAL>.                                        "Old CR 2690 Capital repayments Finance Lease



        ENDLOOP.

        CT_DATA = <LT_FINAL>.



      WHEN 'SUB6' OR 'SUB6A'. "Do CashFlow Subroutine 6 from Calculations Register - Carry Forward
        "Additional Edit to accomodate SUB 7 for indirect settlement


******* LS-SEL *********************************
        LS_SEL-ATTRIBUTE = 'ID'.
        LS_SEL-SIGN = 'I'.
        LS_SEL-OPTION = 'EQ'.
************************************************
****** GET PC MD ********************************

        MASTER_DATA = LO_TOOLS->GET_MD_FROM_CONTEXT('PROFIT_CENTRE').
        ASSIGN MASTER_DATA->* TO <DIMENSION_DATA>.
        MOVE-CORRESPONDING <DIMENSION_DATA> TO LT_PROFIT_CENTRE.

********* READ TARGET ACCOUNTS FROM ACCOUNT TRANSFORMATIONS ******************

        LO_TOOLS->TARGET_ACCS(
            EXPORTING
              IT_CV           = IT_CV
            IMPORTING
              ET_MESSAGE      = ET_MESSAGE
              TARGET_ACCOUNTS = LT_TARGET_DATA ).

******* READ ACCOUNT MD ********************************

        LS_SEL-DIMENSION = 'ACCOUNT'.
        LOOP AT LT_TARGET_DATA INTO LS_TARGET_DATA.
          LS_SEL-LOW = LS_TARGET_DATA-DEST_ACCOUNT.
          APPEND LS_SEL TO LT_SEL.
        ENDLOOP.
        SORT LT_SEL BY LOW.
        DELETE ADJACENT DUPLICATES FROM LT_SEL.

        MASTER_DATA = LO_TOOLS->GET_MD_FROM_CONTEXT(
        EXPORTING
        I_DIMENSION_ID = LS_SEL-DIMENSION
        CHANGING
        LT_SEL = LT_SEL ).

        ASSIGN MASTER_DATA->* TO <DIMENSION_DATA>.
        MOVE-CORRESPONDING <DIMENSION_DATA> TO LT_ACCOUNT.


        LOOP AT LT_ACCOUNT INTO LS_ACCOUNT WHERE SHIFT_VALUE NE ''.
          CHECK LS_ACCOUNT-ID <> 'AC310000'. "make sure AC310000 doesn't get into the table, there is a separate clause for it
          APPEND LS_ACCOUNT TO LT_SHIFT_ACCOUNTS.
        ENDLOOP.

*** Set accounts to read from model************************************
        TVARVC_NAME = I_APPSET_ID && '-' && I_APPL_ID && '-' && 'SUB5'.

        CLEAR: LT_SEL, LS_TARGET_DATA.

        LOOP AT CV_ACCOUNTS-MEMBER INTO WA_MEMBER.

          LS_SEL-DIMENSION = 'ACCOUNT'.
          LS_SEL-LOW = WA_MEMBER.
          APPEND LS_SEL TO LT_SEL.

          LOOP AT LT_TARGET_DATA INTO LS_TARGET_DATA.
            IF LS_TARGET_DATA-ACCOUNT = WA_MEMBER AND LS_TARGET_DATA-DEST_ACCOUNT NE ''.
              LS_SEL-LOW = LS_TARGET_DATA-DEST_ACCOUNT.
              APPEND LS_SEL TO LT_SEL.
            ELSEIF LS_TARGET_DATA-CALC_ID = ZCL_TVARVC=>GET_SINGLE_VALUE( TVARVC_NAME ).
              LS_SEL-LOW = LS_TARGET_DATA-DEST_ACCOUNT.
              APPEND LS_SEL TO LT_SEL.
            ENDIF.
          ENDLOOP.

        ENDLOOP.

        CLEAR WA_MEMBER.

        LOOP AT CV_PROFIT_CENTRE-MEMBER INTO WA_MEMBER.

          LS_SEL-DIMENSION = 'PROFIT_CENTRE'.
          LS_SEL-LOW = WA_MEMBER.
          APPEND LS_SEL TO LT_SEL.

        ENDLOOP.

        LS_SEL-DIMENSION = 'CATEGORY'.
        LS_SEL-LOW = WORKING_CATEGORY.
        APPEND LS_SEL TO LT_SEL.

        READ TABLE IT_CV INTO CV_AUDITTRAIL WITH TABLE KEY DIM_UPPER_CASE = 'AUDITTRAIL'.

        CLEAR WA_MEMBER.

        LOOP AT CV_AUDITTRAIL-MEMBER INTO WA_MEMBER.

          LS_SEL-DIMENSION = 'AUDITTRAIL'.
          LS_SEL-LOW = WA_MEMBER.
          APPEND LS_SEL TO LT_SEL.

        ENDLOOP.

        CLEAR WA_MEMBER.

        LOOP AT FLOWS INTO WA_MEMBER.

          LS_SEL-DIMENSION = 'FLOW'.
          LS_SEL-LOW = WA_MEMBER.
          APPEND LS_SEL TO LT_SEL.


          LOOP AT LT_TARGET_DATA INTO LS_TARGET_DATA.
            IF LS_TARGET_DATA-SUBTABLES = WA_MEMBER AND LS_TARGET_DATA-DEST_SUBTABLES NE ''.
              LS_SEL-LOW = LS_TARGET_DATA-DEST_SUBTABLES.
              APPEND LS_SEL TO LT_SEL.
            ELSEIF LS_TARGET_DATA-CALC_ID = ZCL_TVARVC=>GET_SINGLE_VALUE( TVARVC_NAME ).
              LS_SEL-LOW = LS_TARGET_DATA-DEST_SUBTABLES.
              APPEND LS_SEL TO LT_SEL.
            ENDIF.
          ENDLOOP.

        ENDLOOP.

        CLEAR: WA_MEMBER, LS_TIME.

        LOOP AT LT_TIMESCOPE INTO LS_TIME.

          LS_SEL-DIMENSION = 'TIME'.
          LS_SEL-LOW = LS_TIME-ID.
          APPEND LS_SEL TO LT_SEL.

        ENDLOOP.

        SORT LT_SEL BY LOW.
        DELETE ADJACENT DUPLICATES FROM LT_SEL.
****************************************************************************

        LO_TOOLS->READ_MODEL_DATA(
           EXPORTING
             IT_SEL      = LT_SEL
           IMPORTING
             OUTPUT_DATA = <CT_DATA> ).

        CLEAR CT_DATA.
        MOVE-CORRESPONDING <CT_DATA> TO CT_DATA.

*******************************************************************
* DO THE CALCULATIONS *********************************************
*******************************************************************

        LOOP AT CT_DATA ASSIGNING <LS_REC>.
          <LS_RESULT_REC> = <LS_REC>.
          ASSIGN COMPONENT 'FLOW' OF STRUCTURE <LS_RESULT_REC> TO <LS_FLOW>.
          ASSIGN COMPONENT 'TIME' OF STRUCTURE <LS_RESULT_REC> TO <LS_TIME>.
          ASSIGN COMPONENT 'AUDITTRAIL' OF STRUCTURE <LS_RESULT_REC> TO <LS_AUDITTRAIL>.
          ASSIGN COMPONENT 'CATEGORY' OF STRUCTURE <LS_RESULT_REC> TO <LS_CATEGORY>.
          ASSIGN COMPONENT 'ACCOUNT' OF STRUCTURE <LS_RESULT_REC> TO <LS_ACCOUNT>.
          ASSIGN COMPONENT 'PROFIT_CENTRE' OF STRUCTURE <LS_RESULT_REC> TO <LS_PROFIT_CENTRE>.
          ASSIGN COMPONENT 'SIGNEDDATA' OF STRUCTURE <LS_RESULT_REC> TO <LS_SIGNEDDATA>.

***** PRESERVE VALUE IN CASE LOST ***********************************

          CLEAR: LS_SIGNEDDATA, LS_FLOW.
          LS_SIGNEDDATA = <LS_SIGNEDDATA>.
          LS_FLOW = <LS_FLOW>.


*********************************************************************

************* ADDITIONAL 0 for AC220000 *****************************
          IF <LS_FLOW> = 'F_CASH_CALC'
            AND <LS_ACCOUNT> = 'AC220000'.

            <LS_FLOW> = 'F99'.
            <LS_SIGNEDDATA> = 0.

            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            DELETE CT_DATA.
          ENDIF.

*********************************************************************

***** DO NOT DO THE CALCULATION FOR THE GENERATED FLOWS *************
          IF <LS_FLOW> = 'F_CASH_CALC' AND
            <LS_AUDITTRAIL> = 'IS_INPUT' AND
            <LS_ACCOUNT> = 'AC210000'.
*            <LS_SIGNEDDATA> = 0.
*            APPEND <LS_RESULT_REC> TO <LT_FINAL>.
*            DELETE CT_DATA.
            "   EXIT.
          ELSEIF <LS_FLOW> = 'F_CASH_CALC' .
            DELETE CT_DATA.
            CONTINUE.
          ENDIF.

***** RESET COUNTER ******
          CLEAR COUNTER.
          COUNTER = 1.


************** CALCULATE TIME ID FOR COMPARISON *********************
          YEAR = <LS_TIME>(4).
          MONTH = <LS_TIME>+5(2).
          LS_TIMEID = <LS_TIME>(4) && <LS_TIME>+5(2) && '00'.
*********************************************************************
************** CHECK IF THE ACCOUNT IN RECORD IS THE SHIFTING *******
          "The account and the relevant shift value should be found or not
          "If it's found it will be kept in LS_SHIFT_ACCOUNTS and we will read
          "LS_SHIFT_ACCOUNTS-SHIFT_VALUE will tell us

          CLEAR LS_SHIFT_ACCOUNTS.
          READ TABLE LT_SHIFT_ACCOUNTS INTO LS_SHIFT_ACCOUNTS WITH KEY ID = <LS_ACCOUNT>.
************** CLAUSE FOR THE FIRST PERIOD **************************
          "For the first period only writing closing balance F99

          IF LS_TIMEID > FIRST_PERIOD.
            CHECK <LS_FLOW> <> 'F_OPE'.
          ENDIF.
          <LS_FLOW> = 'F99'.
          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
*********************************************************************


****** AFTER THE FIRST PERIOD IS DONE DO THE REMAINING PERIODS ******

          LOOP AT LT_TIMESCOPE INTO LS_TIME.
            CHECK LS_TIME-TIMEID > LS_TIMEID.


            "There is the special clause for AC310000 account
            IF <LS_ACCOUNT> = 'AC310000'.

              "Do the quarterly settlements only for S101 profit centres
              "ET requested special treatment for PC0001010238 (agreed with JD and DM)

*********** CHANGE 3 DELETE START
*              READ TABLE LT_PROFIT_CENTRE WITH KEY ID = <LS_PROFIT_CENTRE> COMPANY_CODE = 'S101' TRANSPORTING NO FIELDS.
*              "IF SY-SUBRC = 0.
*              IF SY-SUBRC <> 0. "as per Etanev request
*********** CHANGE 3 DELETE END

*********** CHANGE 3 INSERT START
              READ TABLE LT_PROFIT_CENTRE WITH KEY ID = <LS_PROFIT_CENTRE> COMPANY_CODE = 'S101' INTO LS_PROFIT_CENTRE.

              IF LS_PROFIT_CENTRE-ID = 'PC0001010238'. "Do quarterly only for PC0001010238 in company S101
*********** CHANGE 3 INSERT END

**** CHECK WHICH IS THE NEXT QTY PERIOD FOR TAX
                LOOP AT LT_TIMEQRTLY INTO LS_TIMEQRTLY WHERE TIMEID > LS_TIMEID.
                  EXIT.
                ENDLOOP.
**************************************************


                <LS_TIME> = LS_TIME-ID.
                <LS_FLOW> = 'F_OPE'.
                <LS_SIGNEDDATA> = LS_SIGNEDDATA.
                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
                <LS_FLOW> = 'F99'.
                <LS_SIGNEDDATA> = LS_SIGNEDDATA.
                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

                IF  LS_TIMEQRTLY-ID = LS_TIME-ID.
                  <LS_FLOW> = 'F_CASH_CALC'.
                  <LS_TIME> = LS_TIMEQRTLY-ID.
                  <LS_SIGNEDDATA> = LS_SIGNEDDATA * -1.
                  COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
                  <LS_FLOW> = 'F99'.
                  <LS_TIME> = LS_TIMEQRTLY-ID.
                  <LS_SIGNEDDATA> = LS_SIGNEDDATA * -1.
                  COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
                  EXIT.
                ENDIF.
**************************************************

                CLEAR LS_TIMEQRTLY.

                "ELSE.                                      "Old code WO0000002046077
              ELSEIF LS_PROFIT_CENTRE-ID IS NOT INITIAL.    "New code WO0000002046077 (Indirect Tax for non-S101 should not settle in following month)


                <LS_TIME> = LS_TIME-ID.
                <LS_FLOW> = 'F_OPE'.
                <LS_SIGNEDDATA> = LS_SIGNEDDATA.
                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
*                <LS_FLOW> = 'F99'.                         "Fix F99
*                <LS_SIGNEDDATA> = LS_SIGNEDDATA.           "Fix F99
*                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.   "Fix F99
                <LS_FLOW> = 'F_CASH_CALC'.
                <LS_SIGNEDDATA> = LS_SIGNEDDATA * -1.
                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
*                <LS_FLOW> = 'F99'.                                "Fix F99
*                <LS_SIGNEDDATA> = LS_SIGNEDDATA * -1.              "Fix F99
*                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.           "Fix F99
                EXIT.

              ELSE.                                          "New code WO0000002046077 (Regular Carry Forward for the AC310000 non-S101

                <LS_TIME> = LS_TIME-ID.                      "New code WO0000002046077
                <LS_FLOW> = 'F_OPE'.                         "New code WO0000002046077
                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.     "New code WO0000002046077
                <LS_FLOW> = 'F99'.                           "New code WO0000002046077
                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.     "New code WO0000002046077


              ENDIF.

*************** DO THE SEPARATE CALCULATION FOR SHIFTING ACCOUNTS                                                                     "New CR 3101 Intercompany balances settlement
****************IC Accounts 213010 and 213000 Separate block for ease of control in future                                            "New CR 3101 Intercompany balances settlement
*Both accounts could be removed from the list of shifting accounts in future and the following block will stop working                "New CR 3101 Intercompany balances settlement
            ELSEIF <LS_ACCOUNT> = LS_SHIFT_ACCOUNTS-ID "The account is in SHIFT Table and the shift value is specified (could be 0)   "New CR 3101 Intercompany balances settlement
              AND ( LS_FLOW = 'F_ISMVT' OR LS_FLOW = 'F_ITSMVT'  OR LS_FLOW = 'F_PURCHASETAX' OR LS_FLOW = 'F_SALESTAXCR' )           "New CR 3101 Intercompany balances settlement
              AND ( <LS_ACCOUNT> = 'AC213000' OR <LS_ACCOUNT> = 'AC213010' ).                                                         "New CR 3101 Intercompany balances settlement

              <LS_TIME> = LS_TIME-ID.                                                                                                 "New CR 3101 Intercompany balances settlement
              <LS_FLOW> = 'F_OPE'.                                                                                                    "New CR 3101 Intercompany balances settlement
              <LS_SIGNEDDATA> = LS_SIGNEDDATA.                                                                                        "New CR 3101 Intercompany balances settlement
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.                                                                                "New CR 3101 Intercompany balances settlement
              <LS_FLOW> = 'F99'.                                                                                                      "New CR 3101 Intercompany balances settlement
              <LS_SIGNEDDATA> = LS_SIGNEDDATA.                                                                                        "New CR 3101 Intercompany balances settlement
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.                                                                                "New CR 3101 Intercompany balances settlement
              "counter could be used in future to control the shift lag
              "looking at the value of the property. Default currently is 1
              "if 2 is involved
              IF COUNTER = LS_SHIFT_ACCOUNTS-SHIFT_VALUE.                                                                             "New CR 3101 Intercompany balances settlement
                <LS_FLOW> = 'F_CASH_CALC'.                                                                                            "New CR 3101 Intercompany balances settlement
                <LS_SIGNEDDATA> = LS_SIGNEDDATA * -1.                                                                                 "New CR 3101 Intercompany balances settlement
                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.                                                                              "New CR 3101 Intercompany balances settlement
                <LS_FLOW> = 'F99'.                                                                                                    "New CR 3101 Intercompany balances settlement
                <LS_SIGNEDDATA> = LS_SIGNEDDATA * -1.                                                                                 "New CR 3101 Intercompany balances settlement
                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.                                                                              "New CR 3101 Intercompany balances settlement
                EXIT.                                                                                                                 "New CR 3101 Intercompany balances settlement
              ENDIF.                                                                                                                  "New CR 3101 Intercompany balances settlement
              COUNTER = COUNTER + 1.                                                                                                  "New CR 3101 Intercompany balances settlement


*************** Main Accounts *********************************************************
            "ELSEIF <LS_ACCOUNT> = LS_SHIFT_ACCOUNTS-ID AND. "The account is in SHIFT Table.                                          "Old CR 3101 Intercompany balances settlement
            ELSEIF <LS_ACCOUNT> = LS_SHIFT_ACCOUNTS-ID                                                                                "New CR 3101 Intercompany balances settlement
              AND <LS_ACCOUNT> <> 'AC213000' AND <LS_ACCOUNT> <> 'AC213010'.                                                          "New CR 3101 Intercompany balances settlement
              <LS_TIME> = LS_TIME-ID.
              <LS_FLOW> = 'F_OPE'.
              <LS_SIGNEDDATA> = LS_SIGNEDDATA.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
              <LS_FLOW> = 'F99'.
              <LS_SIGNEDDATA> = LS_SIGNEDDATA.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
              "counter could be used in future to control the shift lag
              "looking at the value of the property. Default currently is 1
              "if 2 is involved
              IF COUNTER = LS_SHIFT_ACCOUNTS-SHIFT_VALUE.
                <LS_FLOW> = 'F_CASH_CALC'.
                <LS_SIGNEDDATA> = LS_SIGNEDDATA * -1.
                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
                <LS_FLOW> = 'F99'.
                <LS_SIGNEDDATA> = LS_SIGNEDDATA * -1.
                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
                EXIT.
              ENDIF.
              COUNTER = COUNTER + 1.


*************** THE REST OF CARRY-FORWARD ********************************************
              "Do the normal carry forward for all the standard accounts
              "Which are not shifting and not AC310000

            ELSE.

              <LS_TIME> = LS_TIME-ID.
              <LS_FLOW> = 'F_OPE'.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
              <LS_FLOW> = 'F99'.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
*********************************************

            ENDIF.

          ENDLOOP.

        ENDLOOP.

        "APPEND LINES OF <LT_FINAL> TO CT_DATA.

        CT_DATA = <LT_FINAL>.


      WHEN 'FIN' OR 'FIN_A'. "Final sub to settle AC220000 with F_CASH_CALC

******* GET LIST OF RELEVANT FLOWS ********************************

*** Set accounts to read from model****

        CLEAR: LT_SEL, LS_SEL, LS_TARGET_DATA.

        "  LOOP AT CV_ACCOUNTS-MEMBER INTO WA_MEMBER.

        LS_SEL-ATTRIBUTE = 'ID'.
        LS_SEL-SIGN = 'I'.
        LS_SEL-OPTION = 'EQ'.

        LOOP AT ACCOUNTS INTO WA_MEMBER.

          LS_SEL-DIMENSION = 'ACCOUNT'.
          LS_SEL-LOW = WA_MEMBER.
          APPEND LS_SEL TO LT_SEL.

        ENDLOOP.

        "Append AC2200000 additionally
*          LS_SEL-LOW = 'AC220000'.
*          APPEND LS_SEL TO LT_SEL.

        CLEAR WA_MEMBER.

        LOOP AT CV_PROFIT_CENTRE-MEMBER INTO WA_MEMBER.

          LS_SEL-DIMENSION = 'PROFIT_CENTRE'.
          LS_SEL-LOW = WA_MEMBER.
          APPEND LS_SEL TO LT_SEL.

        ENDLOOP.

        LS_SEL-DIMENSION = 'CATEGORY'.
        LS_SEL-LOW = WORKING_CATEGORY .
        APPEND LS_SEL TO LT_SEL.


        READ TABLE IT_CV INTO CV_AUDITTRAIL WITH TABLE KEY DIM_UPPER_CASE = 'AUDITTRAIL'.

        CLEAR WA_MEMBER.

        LOOP AT CV_AUDITTRAIL-MEMBER INTO WA_MEMBER.

          LS_SEL-DIMENSION = 'AUDITTRAIL'.
          LS_SEL-LOW = WA_MEMBER.
          APPEND LS_SEL TO LT_SEL.

        ENDLOOP.

        CLEAR WA_MEMBER.

*        LOOP AT FLOWS INTO WA_MEMBER.

        LS_SEL-DIMENSION = 'FLOW'.
        LS_SEL-LOW = 'F_CASH_CALC'.
        APPEND LS_SEL TO LT_SEL.
        LS_SEL-LOW = 'F_OPE'.
        APPEND LS_SEL TO LT_SEL.
        LS_SEL-LOW = 'F99'.
        APPEND LS_SEL TO LT_SEL.

*        ENDLOOP.

        CLEAR: WA_MEMBER, LS_TIME.

        LOOP AT LT_TIMESCOPE INTO LS_TIME.

          LS_SEL-DIMENSION = 'TIME'.
          LS_SEL-LOW = LS_TIME-ID.
          APPEND LS_SEL TO LT_SEL.

        ENDLOOP.

        SORT LT_SEL BY LOW.
        DELETE ADJACENT DUPLICATES FROM LT_SEL.
****************************************************************************

        LO_TOOLS->READ_MODEL_DATA(
          EXPORTING
            IT_SEL      = LT_SEL
          IMPORTING
            OUTPUT_DATA = <CT_DATA> ).

        CLEAR CT_DATA.
        MOVE-CORRESPONDING <CT_DATA> TO CT_DATA.

*******************************************************************
* DO THE CALCULATIONS *********************************************
*******************************************************************

        LOOP AT CT_DATA ASSIGNING <LS_RESULT_REC> .
          ASSIGN COMPONENT 'FLOW' OF STRUCTURE <LS_RESULT_REC> TO <LS_FLOW>.
          ASSIGN COMPONENT 'ACCOUNT' OF STRUCTURE <LS_RESULT_REC> TO <LS_ACCOUNT>.
          ASSIGN COMPONENT 'SIGNEDDATA' OF STRUCTURE <LS_RESULT_REC> TO <LS_SIGNEDDATA>. " Change #2

          IF <LS_ACCOUNT> = 'AC220000' AND <LS_FLOW> = 'F_CASH_CALC'.
            DELETE CT_DATA.
            CONTINUE.
          ENDIF.

          IF <LS_ACCOUNT> <> 'AC220000' AND <LS_FLOW> <> 'F_CASH_CALC'.
            DELETE CT_DATA.
            CONTINUE.
          ENDIF.

          <LS_ACCOUNT> = 'AC220000'.

          IF <LS_FLOW> = 'F_CASH_CALC'."""""
            <LS_SIGNEDDATA> = <LS_SIGNEDDATA> * -1. " Change #2.
          ENDIF.""""

        ENDLOOP.

        CREATE DATA LS_RESULT_REC LIKE LINE OF CT_DATA.
        ASSIGN LS_RESULT_REC->* TO <LS_RESULT_REC>.

        LOOP AT CT_DATA ASSIGNING <LS_REC> .
          <LS_RESULT_REC> = <LS_REC>.
          ASSIGN COMPONENT 'FLOW' OF STRUCTURE <LS_RESULT_REC> TO <LS_FLOW>.
          ASSIGN COMPONENT 'ACCOUNT' OF STRUCTURE <LS_RESULT_REC> TO <LS_ACCOUNT>.
          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
        ENDLOOP.

        CT_DATA = <LT_FINAL>.

        CLEAR <LT_FINAL>.

        LOOP AT CT_DATA ASSIGNING <LS_REC>.
          <LS_RESULT_REC> = <LS_REC>.
          ASSIGN COMPONENT 'FLOW' OF STRUCTURE <LS_RESULT_REC> TO <LS_FLOW>.
          ASSIGN COMPONENT 'TIME' OF STRUCTURE <LS_RESULT_REC> TO <LS_TIME>.
          ASSIGN COMPONENT 'CATEGORY' OF STRUCTURE <LS_RESULT_REC> TO <LS_CATEGORY>.
          ASSIGN COMPONENT 'ACCOUNT' OF STRUCTURE <LS_RESULT_REC> TO <LS_ACCOUNT>.
          ASSIGN COMPONENT 'PROFIT_CENTRE' OF STRUCTURE <LS_RESULT_REC> TO <LS_PROFIT_CENTRE>.
          ASSIGN COMPONENT 'SIGNEDDATA' OF STRUCTURE <LS_RESULT_REC> TO <LS_SIGNEDDATA>.

          CHECK <LS_FLOW> NE 'F_OPE'.
          CHECK <LS_FLOW> NE 'F99'.

          CLEAR LS_SIGNEDDATA.
          LS_SIGNEDDATA = <LS_SIGNEDDATA>.
*********************************************************************

************** CALCULATE TIME ID FOR COMPARISON *********************
          YEAR = <LS_TIME>(4).
          MONTH = <LS_TIME>+5(2).
          LS_TIMEID = <LS_TIME>(4) && <LS_TIME>+5(2) && '00'.
*********************************************************************

************** CLAUSE FOR THE FIRST PERIOD **************************
          "For the first period only writing closing balance F99

          IF LS_TIMEID > FIRST_PERIOD.
            CHECK <LS_FLOW> <> 'F_OPE'.
          ENDIF.
          <LS_FLOW> = 'F99'.
          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
*********************************************************************


****** AFTER THE FIRST PERIOD IS DONE DO THE REMAINING PERIODS ******


          LOOP AT LT_TIMESCOPE INTO LS_TIME.
            CHECK LS_TIME-TIMEID > LS_TIMEID.
            "There is the special clause for AC310000 account

*************** THE REST OF CARRY-FORWARD ********************************************
            "Do the normal carry forward for all the standard accounts
            "Which are not shifting and not AC310000

            <LS_TIME> = LS_TIME-ID.
            <LS_FLOW> = 'F_OPE'.
            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            <LS_FLOW> = 'F99'.
            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.


          ENDLOOP.

        ENDLOOP.

        SORT <LT_FINAL>.

        LOOP AT <LT_FINAL> INTO <LS_RESULT_REC>.
          COLLECT <LS_RESULT_REC> INTO CT_DATA.
        ENDLOOP.
        "APPEND LINES OF <LT_FINAL> TO CT_DATA.

    ENDCASE.

  ENDMETHOD.


  METHOD CF_ACT_MAIN.
*&---------------------------------------------------------------------*
*&  Method           CF_ACT_MAIN
*&---------------------------------------------------------------------*
****************************************************************************
* Program type....:  Customer SO Custom Method
* Author...........:   ()
* Date.............: February 2017
* ----------------------------------------------------------------------
* Description......: This method does the main actualisation calculations
*                    for CASHFLOW model. This includes:
*                    a)Copy ACTUAL data to WFORECAST category in (C - 1) period
*                    b)Move FLOW F99 from (C - 1) period to F_OPE of C period
****************************************************************************
* Change Log
*    1. Adding CTA calcs.
*    2. Addition of FX Trans and delta posting on FX Trans
*    3. Defect 2732 resolution. Missing PL98 in scope.
*    4. CTA incorrect sign - flip the sides.
****************************************************************************
* Changed on: 23.08.2017           By: VZ
*
****************************************************************************
INCLUDE ZINC_PLAN_BADI_COMMON_VARS. "Common Variables

*** SET TOOLS INSTANCE *****
    CREATE OBJECT LO_TOOLS
      EXPORTING
        I_ENV_ID = I_APPSET_ID
        I_MOD_ID = I_APPL_ID.
***************************************************************
************** ASSIGN WAs *************************************

 DATA: FX_DATA      TYPE REF TO DATA .

    CREATE DATA LT_FINAL LIKE CT_DATA.
    ASSIGN LT_FINAL->* TO <LT_FINAL>.
    CREATE DATA FX_DATA LIKE CT_DATA.
    ASSIGN FX_DATA->* TO <FX_DATA>.
    CREATE DATA LS_RESULT_REC LIKE LINE OF CT_DATA.
    ASSIGN LS_RESULT_REC->* TO <LS_RESULT_REC>.
    CREATE DATA LS_REC LIKE LINE OF CT_DATA.
    ASSIGN LS_REC->* TO <LS_REC>.
************************ASSIGN STRUCTURE OF INCOMING MODEL ****
    LO_TOOLS->WRITE_MODEL_DATA(
      EXPORTING
        TARGET_MODEL = I_APPL_ID "self structure
        WRITE       = 'N'
     IMPORTING
       LR_DATA     = LR_DATA ).

    ASSIGN LR_DATA->* TO <CT_DATA>.
*****************************************************************************

****** GET TIMESCOPE AND VARIABLES
    LO_TOOLS->TIME(
      EXPORTING
        CATEGORY    = WORKING_CATEGORY
      IMPORTING
       CURRENT_PERIOD   = CURRENT_PERIOD ).

" Get Last Period
LO_TOOLS->PERIOD_OFFSET(
  EXPORTING
    IMPORT_PERIOD = CURRENT_PERIOD
    OFFSET        = -1
  RECEIVING
    EXPORT_PERIOD = PREVIOUS_PERIOD ).


******* LS-SEL *********************************
          LS_SEL-ATTRIBUTE = 'ID'.
          LS_SEL-SIGN = 'I'.
          LS_SEL-OPTION = 'EQ'.
************************************************

          LS_SEL-DIMENSION = 'CATEGORY'.
          LS_SEL-LOW = 'ACTUAL'.
          APPEND LS_SEL TO LT_SEL.

          LS_SEL-DIMENSION = 'FLOW'.
          LS_SEL-LOW = 'F99'.
          APPEND LS_SEL TO LT_SEL.

          LS_SEL-DIMENSION = 'FLOW'.
          LS_SEL-LOW = 'PL99'.
          APPEND LS_SEL TO LT_SEL.

          "Change 3 Insert Start
          LS_SEL-DIMENSION = 'FLOW'.
          LS_SEL-LOW = 'PL98'.
          APPEND LS_SEL TO LT_SEL.
          "Change 3 Insert End


          LS_SEL-DIMENSION = 'TIME'.
          LS_SEL-LOW = PREVIOUS_PERIOD.
          APPEND LS_SEL TO LT_SEL.


        LO_TOOLS->READ_MODEL_DATA(
           EXPORTING
             IT_SEL      = LT_SEL
           IMPORTING
             OUTPUT_DATA = <CT_DATA> ).

    MOVE-CORRESPONDING <CT_DATA> TO CT_DATA.

      LOOP AT CT_DATA ASSIGNING <LS_REC>.
      <LS_RESULT_REC> =  <LS_REC>.
      ASSIGN COMPONENT 'CATEGORY'     OF STRUCTURE <LS_RESULT_REC> TO <LS_CATEGORY>.
      ASSIGN COMPONENT 'RPTCURRENCY'  OF STRUCTURE <LS_RESULT_REC> TO <LS_RPTCURRENCY>.
      ASSIGN COMPONENT 'FLOW'         OF STRUCTURE <LS_RESULT_REC> TO <LS_FLOW>.
      ASSIGN COMPONENT 'TIME'         OF STRUCTURE <LS_RESULT_REC> TO <LS_TIME>.
      ASSIGN COMPONENT 'SIGNEDDATA'   OF STRUCTURE <LS_RESULT_REC> TO <LS_SIGNEDDATA>.

      <LS_CATEGORY> = WORKING_CATEGORY.

      "Write additional F_OPE to C period.
       IF <LS_FLOW> = 'F99'.
         <LS_TIME> = CURRENT_PERIOD.
         <LS_FLOW> = 'F_OPE'.
         COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
       "CTA Calcs
           IF <LS_RPTCURRENCY> = 'LC'.
             <LS_TIME> = CURRENT_PERIOD.
             <LS_FLOW> = 'F_CTA_O'.
             COLLECT <LS_RESULT_REC> INTO <FX_DATA>.
           ENDIF.

        <LS_TIME> = PREVIOUS_PERIOD.
        <LS_FLOW> = 'F99'.
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

         <LS_TIME> = PREVIOUS_PERIOD.
        <LS_FLOW> = 'F_OPE'.
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

       ENDIF.


  "IF <LS_FLOW> = 'PL99'. "Get Cashflow to WFORECAST
  "Change 3 adjustment - adding 'PL98'
   IF <LS_FLOW> = 'PL99' OR <LS_FLOW> = 'PL98'.
     COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
  ENDIF.

    ENDLOOP.

CLEAR CT_DATA.
APPEND LINES OF <LT_FINAL> TO CT_DATA.

    "FX TRANS ON <FX_DATA>
    TRY.
    ME->FXTRANS(
      EXPORTING
        I_APPSET_ID = I_APPSET_ID
        I_APPL_ID   = I_APPL_ID
*        IT_PARAM    =
        IT_CV       = IT_CV
*        FLAG        =
      IMPORTING
        ET_MESSAGE  = ET_MESSAGE
      CHANGING
        CT_DATA     = <FX_DATA>
        ).
     CATCH CX_UJ_CUSTOM_LOGIC .
    ENDTRY.

MOVE-CORRESPONDING <FX_DATA> TO LT_CASHFLOW."CONTAINS F_OPE_C
SORT LT_CASHFLOW BY ACCOUNT AUDITTRAIL CATEGORY
       FLOW PARTNER PROFIT_CENTRE RPTCURRENCY
       TIME .
CLEAR <FX_DATA> .

*****"WE NEED TO POST A DELTA BETWEEN THE ORIGINAL F_OPE_C VS f_OPE

LOOP AT <LT_FINAL> ASSIGNING <LS_REC>.
        <LS_RESULT_REC> =  <LS_REC>.
        ASSIGN COMPONENT 'ACCOUNT'      OF STRUCTURE <LS_RESULT_REC> TO <LS_ACCOUNT>.
        ASSIGN COMPONENT 'AUDITTRAIL'   OF STRUCTURE <LS_RESULT_REC> TO <LS_ENTITY>.
        ASSIGN COMPONENT 'CATEGORY'     OF STRUCTURE <LS_RESULT_REC> TO <LS_CATEGORY>.
        ASSIGN COMPONENT 'FLOW'         OF STRUCTURE <LS_RESULT_REC> TO <LS_FLOW>.
        ASSIGN COMPONENT 'PARTNER'      OF STRUCTURE <LS_RESULT_REC> TO <LS_PARTNER>.
        ASSIGN COMPONENT 'PROFIT_CENTRE'  OF STRUCTURE <LS_RESULT_REC> TO <LS_PROFIT_CENTRE>.
        ASSIGN COMPONENT 'RPTCURRENCY'  OF STRUCTURE <LS_RESULT_REC> TO <LS_RPTCURRENCY>.
        ASSIGN COMPONENT 'TIME'         OF STRUCTURE <LS_RESULT_REC> TO <LS_TIME>.
        ASSIGN COMPONENT 'SIGNEDDATA'   OF STRUCTURE <LS_RESULT_REC> TO <LS_SIGNEDDATA>.

    IF <LS_FLOW> = 'F_OPE'.    " 'F_CTA_O'.

      ELSE.
      CONTINUE.
      ENDIF.
    IF <LS_RPTCURRENCY> = 'LC'.
      CONTINUE.
      ENDIF.

    IF <LS_TIME> = PREVIOUS_PERIOD.
      CONTINUE.
      ENDIF.


    CLEAR WA_CASHFLOW.
    READ TABLE LT_CASHFLOW
    WITH KEY
    ACCOUNT      = <LS_ACCOUNT>
    AUDITTRAIL   = <LS_ENTITY>
    CATEGORY     = <LS_CATEGORY>
    FLOW         = 'F_OPE_C'
    PARTNER      = <LS_PARTNER>
    PROFIT_CENTRE = <LS_PROFIT_CENTRE>
    RPTCURRENCY  = <LS_RPTCURRENCY>
    TIME         = <LS_TIME>
    INTO WA_CASHFLOW
    BINARY SEARCH.
    IF SY-SUBRC = 0.
      <LS_FLOW> = 'F_CTA_O'.
      <LS_SIGNEDDATA> = <LS_SIGNEDDATA> - WA_CASHFLOW-SIGNEDDATA.  "Change 4 Remove
      " <LS_SIGNEDDATA> =  WA_CASHFLOW-SIGNEDDATA - <LS_SIGNEDDATA>. "Change 4 Insert
      COLLECT <LS_RESULT_REC> INTO <FX_DATA>.
      ELSE.
        CONTINUE.
        ENDIF.
  ENDLOOP.

APPEND LINES OF <FX_DATA> TO CT_DATA.


  ENDMETHOD.


  METHOD CLEAR.
*&---------------------------------------------------------------------*
*&  Method           CLEAR
*&---------------------------------------------------------------------*
****************************************************************************
* Program type....:  Customer SO Custom Method
* Author...........:   ()
* Date.............: February 2017
* ----------------------------------------------------------------------
* Description......: This method clears WFORECAST in PREVIOUS (C - 1) period
*                     Accepts to SUB_IDs:
*                     a) If A - clears WFORECAST in (C - 1) period
*                     b) If C - clears WFORECAST in current period
****************************************************************************
* Change Log
****************************************************************************
* Changed on:                 By:
* Description:
****************************************************************************
INCLUDE ZINC_PLAN_BADI_COMMON_VARS. "Common Variables

*** SET TOOLS INSTANCE *****
    CREATE OBJECT LO_TOOLS
      EXPORTING
        I_ENV_ID = I_APPSET_ID
        I_MOD_ID = I_APPL_ID.
***************************************************************
************** ASSIGN WAs *************************************
    CREATE DATA LT_FINAL LIKE CT_DATA.
    ASSIGN LT_FINAL->* TO <LT_FINAL>.
    CREATE DATA LS_RESULT_REC LIKE LINE OF CT_DATA.
    ASSIGN LS_RESULT_REC->* TO <LS_RESULT_REC>.
    CREATE DATA LS_REC LIKE LINE OF CT_DATA.
    ASSIGN LS_REC->* TO <LS_REC>.
************************ASSIGN STRUCTURE OF INCOMING MODEL ****
    LO_TOOLS->WRITE_MODEL_DATA(
      EXPORTING
        TARGET_MODEL = I_APPL_ID "self structure
        WRITE       = 'N'
     IMPORTING
       LR_DATA     = LR_DATA ).

    ASSIGN LR_DATA->* TO <CT_DATA>.
*****************************************************************************

****** GET TIMESCOPE AND VARIABLES
    LO_TOOLS->TIME(
      EXPORTING
        CATEGORY    = WORKING_CATEGORY
      IMPORTING
       CURRENT_PERIOD   = CURRENT_PERIOD ).

" Get Last Period
LO_TOOLS->PERIOD_OFFSET(
  EXPORTING
    IMPORT_PERIOD = CURRENT_PERIOD
    OFFSET        = -1
  RECEIVING
    EXPORT_PERIOD = PREVIOUS_PERIOD ).

IF I_APPL_ID = 'CASHFLOW' OR I_APPL_ID = 'INC_STATEMENT'.
FLAG_ACCOUNTS = LO_TOOLS->GET_PARENT_MD(
      EXPORTING
      PARENT_MBR = ACCOUNT_FLAG_NODE
      I_DIMENSION_ID = 'ACCOUNT').

STAT_ACCOUNTS = LO_TOOLS->GET_PARENT_MD(
      EXPORTING
      PARENT_MBR = ACCOUNT_STAT_NODE
      I_DIMENSION_ID = 'ACCOUNT').

APPEND LINES OF FLAG_ACCOUNTS TO ACCOUNTS.
APPEND LINES OF STAT_ACCOUNTS TO ACCOUNTS.
ENDIF.


******* LS-SEL *********************************
          LS_SEL-ATTRIBUTE = 'ID'.
          LS_SEL-SIGN = 'I'.
          LS_SEL-OPTION = 'EQ'.
************************************************

          LS_SEL-DIMENSION = 'CATEGORY'.
          LS_SEL-LOW = WORKING_CATEGORY.
          APPEND LS_SEL TO LT_SEL.

          LS_SEL-DIMENSION = 'TIME'.
          IF SUB_ID = 'A'. "Remove Category from previous time period "Actuals"
          LS_SEL-LOW = PREVIOUS_PERIOD.
          ELSEIF SUB_ID = 'C'. "Remove Category from current period"
          LS_SEL-LOW = CURRENT_PERIOD.
          ELSE.
         MESSAGE i002(ZCL_BPC_PLANNING_MSG) INTO L_LOG.
         CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).
         MESSAGE e004(ZCL_BPC_PLANNING_MSG) INTO L_LOG WITH SUB_ID.
         CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).
         MESSAGE i003(ZCL_BPC_PLANNING_MSG) INTO L_LOG.
         CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).
         EXIT.
          ENDIF.
          APPEND LS_SEL TO LT_SEL.


        LO_TOOLS->READ_MODEL_DATA(
           EXPORTING
             IT_SEL      = LT_SEL
           IMPORTING
             OUTPUT_DATA = <CT_DATA> ).

    MOVE-CORRESPONDING <CT_DATA> TO CT_DATA.


    LOOP AT CT_DATA ASSIGNING <LS_RESULT_REC>.
      ASSIGN COMPONENT 'ACCOUNT' OF STRUCTURE <LS_RESULT_REC> TO <LS_ACCOUNT>.
      ASSIGN COMPONENT 'CATEGORY' OF STRUCTURE <LS_RESULT_REC> TO <LS_CATEGORY>.
      ASSIGN COMPONENT 'TIME' OF STRUCTURE <LS_RESULT_REC> TO <LS_TIME>.
      ASSIGN COMPONENT 'SIGNEDDATA' OF STRUCTURE <LS_RESULT_REC> TO <LS_SIGNEDDATA>.

IF I_APPL_ID = 'CASHFLOW' OR I_APPL_ID = 'INC_STATEMENT'.
      READ TABLE ACCOUNTS WITH KEY TABLE_LINE = <LS_ACCOUNT> TRANSPORTING NO FIELDS.
      IF SY-SUBRC = 0.
        CONTINUE.
      ELSE.
      <LS_SIGNEDDATA> = 0.
      ENDIF.
ENDIF.

  <LS_SIGNEDDATA> = 0.

    ENDLOOP.


  ENDMETHOD.


  METHOD CLEAR_INACTIVE.
*&---------------------------------------------------------------------*
*&  Method           CLEAR_INACTIVE
*&---------------------------------------------------------------------*
****************************************************************************
* Program type....:  Customer SO Custom Method
* Author...........:   ()
* Date.............: February 2017
* ----------------------------------------------------------------------
* Description......: This method clears inactive entity type objects
*                    from the relevant model.
*                    a) For INC_STATEMENT and CAPEX - ENTITY dim
*                    b) For CASHFLOW - PROFIT_CENTRE
*                    c) For RESOURCE - combined ENTITY and EMPLOYEE status
*****************************************************************************
* Change Log:
*             1. 10.05.2017 - Clear Inactive for resource will do the clear only
*                for WBSs clear inactive employees is handled by start routine
*                within the salary data load. Also, while clearing WBSs only the
*                following accounts will be targeted:
*                TOTAL_MONTHLYVALUES
*                TOTAL_STATACCT
*                RP000431 (ITS Days in Month) and RP000408 (FTE)
****************************************************************************
* Changed on:    10.05.2017             By: VZ
* Description:
****************************************************************************
INCLUDE ZINC_PLAN_BADI_COMMON_VARS. "Common Variables

*** SET TOOLS INSTANCE *****
    CREATE OBJECT LO_TOOLS
      EXPORTING
        I_ENV_ID = I_APPSET_ID
        I_T_PARAM = IT_PARAM
        I_MOD_ID = I_APPL_ID.
***************************************************************
************** ASSIGN WAs *************************************
    CREATE DATA LT_FINAL LIKE CT_DATA.
    ASSIGN LT_FINAL->* TO <LT_FINAL>.
    CREATE DATA LS_RESULT_REC LIKE LINE OF CT_DATA.
    ASSIGN LS_RESULT_REC->* TO <LS_RESULT_REC>.
    CREATE DATA LS_REC LIKE LINE OF CT_DATA.
    ASSIGN LS_REC->* TO <LS_REC>.
************************ASSIGN STRUCTURE OF INCOMING MODEL ****
    LO_TOOLS->WRITE_MODEL_DATA(
      EXPORTING
        TARGET_MODEL = I_APPL_ID "self structure
        WRITE       = 'N'
     IMPORTING
       LR_DATA     = LR_DATA ).

    ASSIGN LR_DATA->* TO <CT_DATA>.
*****************************************************************************

****** GET TIMESCOPE AND VARIABLES
    LO_TOOLS->TIME(
      EXPORTING
        CATEGORY    = WORKING_CATEGORY
      IMPORTING
       TIMESCOPE   = LT_TIMESCOPE ).

******** LS-SEL *********************************
          LS_SEL-ATTRIBUTE = 'ID'.
          LS_SEL-SIGN = 'I'.
          LS_SEL-OPTION = 'EQ'.
************************************************

          LS_SEL-DIMENSION = 'CATEGORY'.
          LS_SEL-LOW = WORKING_CATEGORY.
          APPEND LS_SEL TO LT_SEL.

          LS_SEL-DIMENSION = 'TIME'.

          LOOP AT LT_TIMESCOPE INTO LS_TIME.
          LS_SEL-LOW = LS_TIME-ID.
          APPEND LS_SEL TO LT_SEL.
          ENDLOOP.

          LS_SEL-OPTION = 'CP'.
          LS_SEL-LOW = '*.INP'.
          APPEND LS_SEL TO LT_SEL.

******** LS-SEL *********************************
          LS_SEL-ATTRIBUTE = 'ID'.
          LS_SEL-SIGN = 'I'.
          LS_SEL-OPTION = 'EQ'.
************************************************


CASE I_APPL_ID.

WHEN 'INC_STATEMENT' OR 'CAPEX'.

        MASTER_DATA = LO_TOOLS->GET_INACTIVE_ENTITY( ).

        IF MASTER_DATA IS INITIAL.
          EXIT.
        ENDIF.

        ASSIGN MASTER_DATA->* TO <DIMENSION_DATA>.
        MOVE-CORRESPONDING <DIMENSION_DATA> TO LT_ENTITY.

          LS_SEL-DIMENSION = 'ENTITY'.

        LOOP AT LT_ENTITY INTO LS_ENTITY.
          LS_SEL-LOW = LS_ENTITY-ID.
          APPEND LS_SEL TO LT_SEL.
        ENDLOOP.


WHEN 'CASHFLOW'.

        MASTER_DATA = LO_TOOLS->GET_INACTIVE_ENTITY( ).

        IF MASTER_DATA IS INITIAL.
          EXIT.
        ENDIF.

        ASSIGN MASTER_DATA->* TO <DIMENSION_DATA>.
        MOVE-CORRESPONDING <DIMENSION_DATA> TO LT_PROFIT_CENTRE.

        LS_SEL-DIMENSION = 'PROFIT_CENTRE'.

        LOOP AT LT_PROFIT_CENTRE INTO LS_PROFIT_CENTRE.
          LS_SEL-LOW = LS_PROFIT_CENTRE-ID.
          APPEND LS_SEL TO LT_SEL.
        ENDLOOP.

WHEN 'RESOURCE'.

        MASTER_DATA = LO_TOOLS->GET_INACTIVE_ENTITY( ).

        IF MASTER_DATA IS INITIAL.
          EXIT.
        ENDIF.

        ASSIGN MASTER_DATA->* TO <DIMENSION_DATA>.
        MOVE-CORRESPONDING <DIMENSION_DATA> TO LT_ENTITY.

        LS_SEL-DIMENSION = 'ENTITY'.

        LOOP AT LT_ENTITY INTO LS_ENTITY.
          LS_SEL-LOW = LS_ENTITY-ID.
          APPEND LS_SEL TO LT_SEL.
        ENDLOOP.

****** CHANGE 1 INSERT STARTS **********************
* Get correct accounts

     LS_SEL-DIMENSION = 'RESOURCE_ACCT'.

     LO_TOOLS->GET_PARENT_MD(
          EXPORTING
            PARENT_MBR  = 'TOTAL_MONTHLYVALUES'
            I_DIMENSION_ID = 'RESOURCE_ACCT'
            RECEIVING
            OUTPUT_DATA = ACCOUNTS ).

      LOOP AT ACCOUNTS INTO WA_MEMBER.
        LS_SEL-LOW = WA_MEMBER.
        APPEND LS_SEL TO LT_SEL.
      ENDLOOP.

           LO_TOOLS->GET_PARENT_MD(
          EXPORTING
            PARENT_MBR  = 'TOTAL_STATACCT'
            I_DIMENSION_ID = 'RESOURCE_ACCT'
            RECEIVING
            OUTPUT_DATA = ACCOUNTS ).

      LOOP AT ACCOUNTS INTO WA_MEMBER.
        LS_SEL-LOW = WA_MEMBER.
        APPEND LS_SEL TO LT_SEL.
      ENDLOOP.

        LS_SEL-LOW = 'RP000431'.
        APPEND LS_SEL TO LT_SEL.

        LS_SEL-LOW = 'RP000408'.
        APPEND LS_SEL TO LT_SEL.


****** CHANGE 1 INSERT ENDS ************************

*Commented as part of Change 1
*        MASTER_DATA = LO_TOOLS->GET_INACTIVE_ENTITY( 'X' ).
*        ASSIGN MASTER_DATA->* TO <DIMENSION_DATA>.
*        MOVE-CORRESPONDING <DIMENSION_DATA> TO LT_EMPLOYEE.
*
*        LS_SEL-DIMENSION = 'EMPLOYEE'.
*
*        LOOP AT LT_EMPLOYEE INTO LS_EMPLOYEE.
*          LS_SEL-LOW = LS_EMPLOYEE-ID.
*          APPEND LS_SEL TO LT_SEL.
*        ENDLOOP.


ENDCASE.


        LO_TOOLS->READ_MODEL_DATA(
           EXPORTING
             IT_SEL      = LT_SEL
           IMPORTING
             OUTPUT_DATA = <CT_DATA> ).

    MOVE-CORRESPONDING <CT_DATA> TO CT_DATA.


    LOOP AT CT_DATA ASSIGNING <LS_RESULT_REC>.

      ASSIGN COMPONENT 'ACCOUNT' OF STRUCTURE <LS_RESULT_REC> TO <LS_ACCOUNT>.
      ASSIGN COMPONENT 'CATEGORY' OF STRUCTURE <LS_RESULT_REC> TO <LS_CATEGORY>.
      ASSIGN COMPONENT 'TIME' OF STRUCTURE <LS_RESULT_REC> TO <LS_TIME>.
      ASSIGN COMPONENT 'SIGNEDDATA' OF STRUCTURE <LS_RESULT_REC> TO <LS_SIGNEDDATA>.

      <LS_SIGNEDDATA> = 0.

    ENDLOOP.

 "We will try to write back zeroes to Resource model and trigger recalculation and push to IS as the result of
 "default trigger.

IF I_APPL_ID = 'RESOURCE'.

    LO_TOOLS->WRITE_MODEL_DATA(
      EXPORTING
     "  TARGET_MODEL     = I_APPL_ID
       INPUT_DATA  = CT_DATA
       WRITE  = 'Y'
      IMPORTING
       ET_MESSAGE  = ET_MESSAGE
     "  ET_ERROR_RECORDS = ET_ERROR_RECORDS
       ).

    CLEAR CT_DATA.

ENDIF.

ENDMETHOD.


  METHOD CREATE_SNAPSHOT.
*&---------------------------------------------------------------------*
*&  Method           CREATE_SNAPSHOT
*&---------------------------------------------------------------------*
****************************************************************************
* Program type....:  Customer SO Custom Method
* Author...........:   ()
* Date.............: February 2017
* ----------------------------------------------------------------------
* Description......: Method is used to create snapshot from WFORECAST
*                    category into snapshot category (identified by relevant
*                    property on CATEGORY dimension). Could be used for multiple
*                    models simultaneously, provided necessary filter logic
*                    adjusted.
*****************************************************************************
* Change Log: 1. Changes to timescope per defect 2706.
*
****************************************************************************
* Changed on:         09.05.2017        By: VZ
* Description:
****************************************************************************
INCLUDE ZINC_PLAN_BADI_COMMON_VARS. "Common Variables


*** SET TOOLS INSTANCE *****
    CREATE OBJECT LO_TOOLS
      EXPORTING
        I_ENV_ID = I_APPSET_ID
        I_MOD_ID = I_APPL_ID.
***************************************************************
************** ASSIGN WAs *************************************
    CREATE DATA LT_FINAL LIKE CT_DATA.
    ASSIGN LT_FINAL->* TO <LT_FINAL>.
    CREATE DATA LS_RESULT_REC LIKE LINE OF CT_DATA.
    ASSIGN LS_RESULT_REC->* TO <LS_RESULT_REC>.
    CREATE DATA LS_REC LIKE LINE OF CT_DATA.
    ASSIGN LS_REC->* TO <LS_REC>.
************************ASSIGN STRUCTURE OF INCOMING MODEL ****

    LO_TOOLS->WRITE_MODEL_DATA(
      EXPORTING
        TARGET_MODEL = I_APPL_ID "self structure
        WRITE       = 'N'
     IMPORTING
       LR_DATA     = LR_DATA ).

    ASSIGN LR_DATA->* TO <CT_DATA>.
*****************************************************************************

****** GET TIMESCOPE AND VARIABLES
    LO_TOOLS->TIME(
      EXPORTING
        CATEGORY    = WORKING_CATEGORY
      IMPORTING
       TIMESCOPE   = LT_TIMESCOPE
       CURRENT_PERIOD   = CURRENT_PERIOD ).

**** CHANGE 1 INSERT STARTS *************************

CLEAR LT_TIMESCOPE.
YEAR = CURRENT_PERIOD(4).

DO 2 TIMES.

IF sy-index = 2.
  YEAR = YEAR + 1.
ENDIF.

DO 12 times.

  IF sy-index < 10.
    LS_TIME-ID = YEAR && '.0' && sy-index.
    APPEND LS_TIME TO LT_TIMESCOPE.
  ELSE.
    LS_TIME-ID = YEAR && '.' && sy-index.
    APPEND LS_TIME TO LT_TIMESCOPE.
  ENDIF.

ENDDO.
ENDDO.

**** CHANGE 1 INSERT ENDS  **************************

    LS_TIME-ID = 'XXXX.INP'.
    APPEND LS_TIME TO LT_TIMESCOPE.

    FCST_SNAPSHOT = LO_TOOLS->GET_SNAPSHOT_ID( CURRENT_PERIOD ).

    IF FCST_SNAPSHOT IS INITIAL.

      MESSAGE E005(ZCL_BPC_PLANNING_MSG) INTO L_LOG WITH 'COPYFORECAST' CURRENT_PERIOD.

      LS_MESSAGE-MSGTY = 'E'.
      LS_MESSAGE-MSGV1 = I_APPL_ID.
      LS_MESSAGE-MESSAGE = L_LOG.
      APPEND LS_MESSAGE TO ET_MESSAGE.
      CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).
      RAISE EXCEPTION TYPE CX_UJ_CUSTOM_LOGIC.

    ENDIF.

******* LS-SEL *********************************
    LS_SEL-ATTRIBUTE = 'ID'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
************************************************

    LS_SEL-DIMENSION = 'CATEGORY'.
    LS_SEL-LOW = WORKING_CATEGORY.
    APPEND LS_SEL TO LT_SEL.

    LS_SEL-DIMENSION = 'CATEGORY'.
    LS_SEL-LOW = FCST_SNAPSHOT.
    APPEND LS_SEL TO LT_SEL.

    LS_SEL-DIMENSION = 'TIME'.
    LOOP AT LT_TIMESCOPE INTO LS_TIME.
      LS_SEL-LOW = LS_TIME-ID.
      APPEND LS_SEL TO LT_SEL.

    ENDLOOP.

    READ TABLE IT_CV INTO CV_AUDITTRAIL WITH TABLE KEY DIM_UPPER_CASE = 'AUDITTRAIL'.

    CLEAR WA_MEMBER.

    LOOP AT CV_AUDITTRAIL-MEMBER INTO WA_MEMBER.

      LS_SEL-DIMENSION = 'AUDITTRAIL'.
      LS_SEL-LOW = WA_MEMBER.
      APPEND LS_SEL TO LT_SEL.

    ENDLOOP.


    LO_TOOLS->READ_MODEL_DATA(
       EXPORTING
         IT_SEL      = LT_SEL
       IMPORTING
         OUTPUT_DATA = <CT_DATA> ).

    MOVE-CORRESPONDING <CT_DATA> TO CT_DATA.


    LOOP AT CT_DATA ASSIGNING <LS_RESULT_REC>.

      ASSIGN COMPONENT 'ACCOUNT' OF STRUCTURE <LS_RESULT_REC> TO <LS_ACCOUNT>.
      ASSIGN COMPONENT 'CATEGORY' OF STRUCTURE <LS_RESULT_REC> TO <LS_CATEGORY>.
      ASSIGN COMPONENT 'TIME' OF STRUCTURE <LS_RESULT_REC> TO <LS_TIME>.
      ASSIGN COMPONENT 'SIGNEDDATA' OF STRUCTURE <LS_RESULT_REC> TO <LS_SIGNEDDATA>.

      IF <LS_CATEGORY> = FCST_SNAPSHOT.
        <LS_SIGNEDDATA> = 0.
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
      ELSEIF <LS_CATEGORY> = WORKING_CATEGORY.
        <LS_CATEGORY> = FCST_SNAPSHOT.
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
      ENDIF.

    ENDLOOP.

* Copy Comments
    LO_TOOLS->COPY_COMMENTS(
      EXPORTING
        LV_CATEGORY_FROM = WORKING_CATEGORY
        LV_CATEGORY_TO   = FCST_SNAPSHOT
*      LV_TIME_FROM     =
*      LV_TIME_TO       =
        TIMESCOPE        = LT_TIMESCOPE
      IMPORTING
       ET_MESSAGE       = ET_MESSAGE
         ).

CT_DATA = <LT_FINAL>.

  ENDMETHOD.


  METHOD FXTRANS.
*&---------------------------------------------------------------------*
****************************************************************************
* Program type....:  Customer SO Custom Method
* Author...........:   ()
* Date.............: January 2017
* ----------------------------------------------------------------------
* Description......: This method handles all the relevant calculations
*                    for Currency Translation
*
*
****************************************************************************
* Change Log
****************************************************************************
* Changed on:                 By:
* Description:
****************************************************************************

    "Internal table def for Dimension type entity,it will be used for ENTITY and PROFIT_CENTRE
    DATA: BEGIN OF LS_ENTITY,
            ID           TYPE CHAR32,
            CURRENCY     TYPE CHAR32,
            REG_CURRENCY TYPE CHAR3,
          END OF LS_ENTITY.
    "Internal table def for Dimension type account, it will be used for ASSET_CLASS, RESOURCE_ACCT, ACCOUNT and RISKOPS_ACCT
    DATA: BEGIN OF LS_ACCOUNT,
            ID       TYPE CHAR32,
            RATETYPE TYPE CHAR32,
          END OF LS_ACCOUNT.
    "Internal table def for Dimension type subtables, it will be used for FLOW
    DATA: BEGIN OF LS_FLOW,
            ID TYPE CHAR32,
*      PARENTH1   TYPE CHAR32,
          END OF LS_FLOW.
    "Internal table def for model RATES
    DATA: BEGIN OF LS_RATES_DATA,
            CATEGORY      TYPE CHAR32,
            INPUTCURRENCY TYPE CHAR32,
            R_ACCOUNT     TYPE CHAR32,
            R_ENTITY      TYPE CHAR32,
            TIME          TYPE CHAR32,
            SIGNEDDATA    TYPE UJ_SDATA,
          END OF LS_RATES_DATA.

    DATA: LT_SEL         TYPE UJ0_T_SEL,
          LS_SEL         TYPE UJ0_S_SEL,
          LT_ENTITY      LIKE SORTED TABLE OF LS_ENTITY WITH UNIQUE KEY ID,
          LT_ACCOUNT     LIKE SORTED TABLE OF LS_ACCOUNT WITH UNIQUE KEY ID,
          LT_RATES_DATA  LIKE STANDARD TABLE OF LS_RATES_DATA,
          LO_TOOLS       TYPE REF TO ZCL_BPC_TOOLS_CLASSIC, "INSTANCE REFERENCE
          LR_DATA        TYPE REF TO DATA,
          LO_APPL_MGR    TYPE REF TO IF_UJA_APPLICATION_MANAGER,
          LS_APPLICATION TYPE UJA_S_APPLICATION,
          LS_DIMENSIONS  TYPE UJA_S_DIMENSION,
          LT_MESSAGE     TYPE UJ0_T_MESSAGE,
          ORIGAMOUNT     TYPE UJ_SDATA,
          GBPAMOUNT      TYPE UJ_SDATA,
          GBPAMOUNT_PREV TYPE UJ_SDATA,
          GBPAMOUNT_CLO  TYPE UJ_SDATA,
          REGAMOUNT      TYPE UJ_SDATA,
          REGAMOUNT_PREV TYPE UJ_SDATA,
          REGAMOUNT_CLO  TYPE UJ_SDATA,
          PREV_PER       TYPE CHAR32,
          CURR_PER       TYPE CHAR32,
          YEAR           TYPE CHAR32,
          PERIOD         TYPE C LENGTH 2,
          IYEAR          TYPE INT4,
          IPERIOD        TYPE N LENGTH 2.
    .
    DATA: MD_ENTITY   TYPE REF TO DATA,
          MD_ACCOUNT  TYPE REF TO DATA,
          ACCOUNT_DIM TYPE CHAR32,
          ENTITY_DIM  TYPE CHAR32,
          L_LOG       TYPE STRING..

    FIELD-SYMBOLS: <ENTITY_DIM_DATA>  TYPE STANDARD TABLE,
                   <ACCOUNT_DIM_DATA> TYPE STANDARD TABLE.
*********************************************************
********************* DATA VARIABLES *************************
    DATA: LT_FINAL      TYPE REF TO DATA,
          LS_REC        TYPE REF TO DATA,
          LS_RESULT_REC TYPE REF TO DATA,
          LT_RATES      TYPE REF TO DATA.

    FIELD-SYMBOLS: <LT_FINAL>      TYPE STANDARD TABLE,
                   <LS_REC>        TYPE ANY,
                   <LS_RESULT_REC> TYPE ANY.

    FIELD-SYMBOLS:
                   <CURRENCY>       TYPE ANY,
                   <RATETYPE>       TYPE ANY,
                   <INPUTGBP>       TYPE ANY,
                   <OUTPUTLC>       TYPE ANY,
                   <LS_ASSET>       TYPE ANY,
                   <LS_ACCOUNT>     TYPE ANY,
                   <LS_AUDITTRAIL>  TYPE ANY,
                   <LS_CATEGORY>    TYPE ANY,
                   <LS_ENTITY>      TYPE ANY,
                   <LS_FLOW>        TYPE ANY,
                   <LS_RPTCURRENCY> TYPE ANY,
                   <LS_TIMEID>      TYPE ANY,
                   <LS_SIGNEDDATA>  TYPE ANY.

**************************************************************
    DATA: CV_CATEGORY       TYPE UJK_S_CV,
          CV_TIME           TYPE UJK_S_CV,
          LT_CV_TIME        TYPE UJA_T_DIM_MEMBER,
          WA_MEMBER         TYPE UJ_DIM_MEMBER,
          WORKING_CATEGORY  TYPE STRING ,"UJ_DIM_MEMBER, "BUDGET OR WFORECAST
          CATEGORY_YEAR     TYPE UJ_DIM_MEMBER. "YEAR INDICATED IN LT_CATEGORY

    READ TABLE IT_CV INTO CV_CATEGORY WITH TABLE KEY DIM_UPPER_CASE = 'CATEGORY'.
**** Cant process two categoroes simultaneously due to time dim restrictions in WFORECAST and BUDGET
    LOOP AT CV_CATEGORY-MEMBER INTO WA_MEMBER.
      IF SY-TABIX > 1.
        CLEAR: ET_MESSAGE, L_LOG.
        L_LOG = 'DO NOT SEND MULTIPLE CATEGORIES SIMULTANEOUSLY! RESTRICT IN PACKAGE OR IN INPUT SCHEDULE.'.
        CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).

      ENDIF.
    ENDLOOP.
    WORKING_CATEGORY = WA_MEMBER. "either WFORECAST OR BUDGET

    IF FLAG IS INITIAL.
        READ TABLE IT_CV INTO CV_TIME WITH TABLE KEY DIM_UPPER_CASE = 'TIME'.
        LOOP AT CV_TIME-MEMBER INTO WA_MEMBER.
          APPEND WA_MEMBER TO LT_CV_TIME.
        ENDLOOP.
    ENDIF.

************************ASSIGN STRUCTURE OF RATES MODEL ****
    FIELD-SYMBOLS: <RATES_DATA> TYPE STANDARD TABLE,
                   <DATA_LINE>  TYPE ANY.

*** SET TOOLS INSTANCE *****
    CREATE OBJECT LO_TOOLS
      EXPORTING
        I_ENV_ID = I_APPSET_ID
        I_MOD_ID = I_APPL_ID.

***************************************
    LO_TOOLS->WRITE_MODEL_DATA(
      EXPORTING
        TARGET_MODEL = 'RATES' "self structure
        WRITE       = 'N'
     IMPORTING
       LR_DATA     = LR_DATA ).

    ASSIGN LR_DATA->* TO <RATES_DATA>.

**********************************************************************
***********************GET ENTITY AND ACCOUNT DIM ********************
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
      IF LS_DIMENSIONS-DIM_TYPE = 'A'.
        ACCOUNT_DIM = LS_DIMENSIONS-DIMENSION.
      ENDIF.
    ENDLOOP.
**********************************************************************
************ ASSIGN WAs **********************************************
    CREATE DATA LT_FINAL LIKE CT_DATA.
    ASSIGN LT_FINAL->* TO <LT_FINAL>.
    CREATE DATA LS_RESULT_REC LIKE LINE OF CT_DATA.
    ASSIGN LS_RESULT_REC->* TO <LS_RESULT_REC>.
    CREATE DATA LS_REC LIKE LINE OF CT_DATA.
    ASSIGN LS_REC->* TO <LS_REC>.
    CREATE DATA LT_RATES LIKE <RATES_DATA>.
******************************************************************

***** GET ENTITY TYPE DIM MD *************************************
    LS_SEL-DIMENSION = ENTITY_DIM."'ENTITY'.
    "APPEND LS_SEL TO LT_SEL.

    LO_TOOLS->READ_MASTER_DATA(
       EXPORTING
         I_DIMENSION_ID   = LS_SEL-DIMENSION
       IMPORTING
         OUTPUT_R_DATA = MD_ENTITY
       CHANGING
         LT_SEL        = LT_SEL ).

    ASSIGN MD_ENTITY->* TO <ENTITY_DIM_DATA>.
    MOVE-CORRESPONDING <ENTITY_DIM_DATA> TO LT_ENTITY.
*******************************************************************

***** GET ACCOUNT TYPE DIM MD *************************************
    LS_SEL-DIMENSION = ACCOUNT_DIM."'ASSET_CLASS'.
    "APPEND LS_SEL TO LT_SEL.

    LO_TOOLS->READ_MASTER_DATA(
       EXPORTING
         I_DIMENSION_ID   = LS_SEL-DIMENSION
       IMPORTING
         OUTPUT_R_DATA = MD_ACCOUNT
       CHANGING
         LT_SEL        = LT_SEL ).

    ASSIGN MD_ACCOUNT->* TO <ACCOUNT_DIM_DATA>.
    MOVE-CORRESPONDING <ACCOUNT_DIM_DATA> TO LT_ACCOUNT.
****************************************************************
***** Time structure definition ********************
    DATA: BEGIN OF LS_TIME,
            ID        TYPE CHAR32,
            CURRMONTH TYPE CHAR32,
            BUDMONTH  TYPE CHAR32,
            TIMEID    TYPE CHAR32,
            VAT_QLY   TYPE CHAR32,
          END OF LS_TIME.
****************************************************

DATA: LT_TIMESCOPE      LIKE SORTED TABLE OF LS_TIME WITH UNIQUE KEY ID,
      CURRENT_PERIOD    TYPE UJ_DIM_MEMBER.
***** GET TIME REFERENCE *************************************
IF WORKING_CATEGORY = 'WFORECAST'.
    LO_TOOLS->TIME( EXPORTING
       CATEGORY       = WORKING_CATEGORY
     IMPORTING
       TIMESCOPE      = LT_TIMESCOPE
*    FIRST_PERIOD   =
*    LAST_PERIOD    =
       CURRENT_PERIOD = CURRENT_PERIOD ).
    ENDIF.

***************** GET RATES CUBE DATA *********************


    CLEAR: LT_SEL, LS_SEL.
    LS_SEL-DIMENSION = 'R_ENTITY'.
    LS_SEL-ATTRIBUTE = 'ID'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = 'Global'.
    APPEND LS_SEL TO LT_SEL.

    CLEAR: LT_SEL, LS_SEL.
    LS_SEL-DIMENSION = 'CATEGORY'.
    LS_SEL-ATTRIBUTE = 'ID'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = WORKING_CATEGORY.
    APPEND LS_SEL TO LT_SEL.

  IF FLAG IS INITIAL.
    LOOP AT LT_CV_TIME INTO WA_MEMBER.
      LS_SEL-DIMENSION = 'TIME'.
      LS_SEL-ATTRIBUTE = 'ID'.
      LS_SEL-SIGN = 'I'.
      LS_SEL-OPTION = 'EQ'.
      LS_SEL-LOW = WA_MEMBER.
      APPEND LS_SEL TO LT_SEL.
    ENDLOOP.

    ELSE.
      LOOP AT CT_DATA ASSIGNING <LS_REC>.
        <LS_RESULT_REC> = <LS_REC>.
        ASSIGN COMPONENT 'TIME'        OF STRUCTURE <LS_RESULT_REC> TO <LS_TIMEID>.
        LS_SEL-DIMENSION = 'TIME'.
        LS_SEL-ATTRIBUTE = 'ID'.
        LS_SEL-SIGN = 'I'.
        LS_SEL-OPTION = 'EQ'.
        LS_SEL-LOW = <LS_TIMEID>.
        APPEND LS_SEL TO LT_SEL.
      ENDLOOP.

      ENDIF.

    LO_TOOLS->READ_MODEL_DATA(
      EXPORTING
        READ_MODEL   = 'RATES'
        IT_SEL      = LT_SEL
      IMPORTING
        OUTPUT_DATA = <RATES_DATA> ).

    CLEAR LT_RATES_DATA.
    MOVE-CORRESPONDING <RATES_DATA> TO LT_RATES_DATA.
    SORT LT_RATES_DATA BY CATEGORY INPUTCURRENCY R_ACCOUNT TIME.
***********************************************

    IF I_APPL_ID = 'CASHFLOW'. "Start APPL ID Check for CF
      "DO CASHFLOW CALC FX HERE

************** GET CHILDREN OF F_MOVE ***********************
      DATA : LT_FLOW_F_MOVE  TYPE UJA_T_DIM_MEMBER.


      LS_SEL-DIMENSION = 'FLOW'."'ASSET_CLASS'.

      LT_FLOW_F_MOVE = LO_TOOLS->GET_PARENT_MD(
        EXPORTING
        PARENT_MBR = 'F_MOVE'
        I_DIMENSION_ID = LS_SEL-DIMENSION ).

      SORT LT_FLOW_F_MOVE .
**************************************************************

*************
      LOOP AT CT_DATA ASSIGNING <LS_REC>.
        CLEAR:ORIGAMOUNT, GBPAMOUNT  , GBPAMOUNT_PREV  ,  GBPAMOUNT_CLO   ,
          REGAMOUNT       ,REGAMOUNT_PREV  ,  REGAMOUNT_CLO   ,      PREV_PER        ,
          CURR_PER        ,  YEAR        ,     PERIOD        .

        <LS_RESULT_REC> = <LS_REC>.
        ASSIGN COMPONENT ACCOUNT_DIM   OF STRUCTURE <LS_RESULT_REC> TO <LS_ACCOUNT>.
        ASSIGN COMPONENT ENTITY_DIM    OF STRUCTURE <LS_RESULT_REC> TO <LS_ENTITY>.
        ASSIGN COMPONENT 'FLOW'        OF STRUCTURE <LS_RESULT_REC> TO <LS_FLOW>.
        ASSIGN COMPONENT 'CATEGORY'    OF STRUCTURE <LS_RESULT_REC> TO <LS_CATEGORY>.
        ASSIGN COMPONENT 'TIME'        OF STRUCTURE <LS_RESULT_REC> TO <LS_TIMEID>.
        ASSIGN COMPONENT 'RPTCURRENCY' OF STRUCTURE <LS_RESULT_REC> TO <LS_RPTCURRENCY>.
        ASSIGN COMPONENT 'SIGNEDDATA'  OF STRUCTURE <LS_RESULT_REC> TO <LS_SIGNEDDATA>.
        ORIGAMOUNT =  <LS_SIGNEDDATA>.

        "ONLY CONVERT RECORDS IF THEY ARE LC
        IF <LS_RPTCURRENCY> <> 'LC'.
          CONTINUE.
        ENDIF.

        "ONLY CONVERT RECORDS TIME IS PROPER
        IF <LS_TIMEID>+5(2) = 'IN'.
          CONTINUE.
        ENDIF.

        "DERIVE PREVIOUS MONTH OF THE RECORD BEING PROCESSED
        CURR_PER = <LS_TIMEID>.
        YEAR = CURR_PER(4).
        PERIOD = CURR_PER+5(2).
        IYEAR = YEAR.
        IPERIOD = PERIOD.
        IF PERIOD = '01'.
          IYEAR = IYEAR - 1 .
          PREV_PER = IYEAR && '.' && '12'.
        ELSE.
          IPERIOD = IPERIOD - 1.
          PREV_PER = YEAR && '.' && IPERIOD.
        ENDIF.
        "QUERY THE CURRENCY AND REG_CURRENCY OF THE PROFIT_CENTRE ENTITY
        CLEAR LS_ENTITY.
        READ TABLE LT_ENTITY WITH TABLE KEY
        ID = <LS_ENTITY>
        INTO LS_ENTITY.
        IF SY-SUBRC = 0 .
          "CHECK IF CURRENCY & REG_CURRENCY ARE NON BLANK
          IF LS_ENTITY-CURRENCY IS INITIAL OR
            LS_ENTITY-REG_CURRENCY IS INITIAL.
            l_log = 'Check Currency or reg_currency property of ' && ' ' && <LS_ENTITY>.
            cl_ujk_logger=>log( i_object = l_log ).
            CONTINUE.
          ENDIF.
        ELSE.
            l_log = 'Check Currency or reg_currency property of ' && ' ' && <LS_ENTITY>.
            cl_ujk_logger=>log( i_object = l_log ).
            CONTINUE.
        ENDIF.
        "PROCESS BY FLOW VALUE
        CASE <LS_FLOW>.
          WHEN 'F_OPE'.

            "IF WORKING_CATEGORY = 'BUDGET'.

            "PROCES FOR FLOW = F_OPE
            "1.MAKE GBP CONV CLO RATE PREVIOUS MONTH ;FLOW = F_OPE
            IF LS_ENTITY-CURRENCY = 'GBP'.
              "NO NEED TO CONVERT JUST WRITE AS IS BUT CHANGE RPTCURRENCY
              <LS_SIGNEDDATA> = ORIGAMOUNT.
              <LS_RPTCURRENCY> = 'GBP'.
              GBPAMOUNT_PREV = <LS_SIGNEDDATA>.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            ELSE.
              CLEAR LS_RATES_DATA.
              READ TABLE LT_RATES_DATA WITH KEY
              CATEGORY = <LS_CATEGORY>
              INPUTCURRENCY = LS_ENTITY-CURRENCY
              R_ACCOUNT = 'CLO'
              TIME = PREV_PER
              INTO LS_RATES_DATA.
              IF SY-SUBRC = 0.
                <LS_RPTCURRENCY> = 'GBP'.
                <LS_SIGNEDDATA> = ORIGAMOUNT / LS_RATES_DATA-SIGNEDDATA .
                GBPAMOUNT_PREV =  <LS_SIGNEDDATA> .
                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
              ELSE.
                CONTINUE.
              ENDIF.
            ENDIF.
            "2.MAKE REG_CURR CLO RATE PREVIOUS MONTH ;FLOW = F_OPE
            "APPEND <LS_RESULT_REC> TO <LT_FINAL>.
            IF LS_ENTITY-REG_CURRENCY = 'GBP'.
              "NO NEED TO WRITE TO DATA
              CONTINUE.
            ELSEIF LS_ENTITY-CURRENCY = LS_ENTITY-REG_CURRENCY.
              "NO NEED TO CONVERT JUST CHANGE RPTCURRENCY
              <LS_SIGNEDDATA> = ORIGAMOUNT.
              <LS_RPTCURRENCY> = LS_ENTITY-REG_CURRENCY.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            ELSE.
              "USE THE GBPAMOUNT_PREV TO CONVERT TO REG_CURRENCY
              CLEAR LS_RATES_DATA.
              READ TABLE LT_RATES_DATA WITH KEY
               CATEGORY = <LS_CATEGORY>
               INPUTCURRENCY = LS_ENTITY-REG_CURRENCY
               R_ACCOUNT = 'CLO'
               TIME = PREV_PER
               INTO LS_RATES_DATA.
              IF SY-SUBRC = 0.
                <LS_RPTCURRENCY> = LS_ENTITY-REG_CURRENCY.
                <LS_SIGNEDDATA> = GBPAMOUNT_PREV * LS_RATES_DATA-SIGNEDDATA .
                REGAMOUNT_PREV = <LS_SIGNEDDATA>.
                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
              ELSE.
                CONTINUE.
              ENDIF.
            ENDIF.

            IF WORKING_CATEGORY = 'WFORECAST' AND CURRENT_PERIOD = <LS_TIMEID>.
              "DO NOT COMPUTE F_CTA_O FOR WFORECAST WHEN TIME IS C
              ELSE.
            "PROCES FOR FLOW = F_OPE
            "3.MAKE GBP CONV CLO RATE DELTA MONTH - LAST MONTH ;CHANGE FLOW = F_CTA_O
            IF LS_ENTITY-CURRENCY = 'GBP'.
              "NO NEED TO CONVERT JUST WRITE AS IS BUT CHANGE RPTCURRENCY
              CLEAR <LS_SIGNEDDATA>.
              <LS_RPTCURRENCY> = 'GBP'.
              <LS_FLOW> = 'F_CTA_O'.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            ELSE.
              "GBPAMOUNT_PREV CONTAINS PREVIOUS CLO AMOUNT FOR THIS SECTION
              "CALC FOR GBP FOR CURRENT CLO AMOUNT
              "DEDUCT CURRENT - PREVIOUS.
              CLEAR LS_RATES_DATA.
              READ TABLE LT_RATES_DATA WITH KEY
              CATEGORY = <LS_CATEGORY>
              INPUTCURRENCY = LS_ENTITY-CURRENCY
              R_ACCOUNT = 'CLO'
              TIME = CURR_PER
              INTO LS_RATES_DATA.
              IF SY-SUBRC = 0.
                <LS_RPTCURRENCY> = 'GBP'.
                <LS_FLOW> = 'F_CTA_O'.
                GBPAMOUNT = ORIGAMOUNT / LS_RATES_DATA-SIGNEDDATA .
                <LS_SIGNEDDATA> = GBPAMOUNT - GBPAMOUNT_PREV.

                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
              ELSE.
                CONTINUE.
              ENDIF.
            ENDIF.

            "4.MAKE REG_CURR CLO RATE DELTA MONTH - LAST MONTH ;CHANGE FLOW = F_CTA_O
            IF LS_ENTITY-REG_CURRENCY = 'GBP'.
              "NO NEED TO WRITE TO DATA
              CONTINUE.
            ELSEIF LS_ENTITY-CURRENCY = LS_ENTITY-REG_CURRENCY.
              "NO NEED TO CONVERT JUST CHANGE RPTCURRENCY
              CLEAR <LS_SIGNEDDATA>.
              <LS_RPTCURRENCY> = LS_ENTITY-REG_CURRENCY.
              <LS_FLOW> = 'F_CTA_O'.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            ELSE.
              "USE THE GBPAMOUNT_PREV TO CONVERT TO REG_CURRENCY
              "USE THE GBPAMOUNT TO CONVERT TO REG_CURRENCY
              CLEAR LS_RATES_DATA.
              READ TABLE LT_RATES_DATA WITH KEY
               CATEGORY = <LS_CATEGORY>
               INPUTCURRENCY = LS_ENTITY-REG_CURRENCY
               R_ACCOUNT = 'CLO'
               TIME = CURR_PER
               INTO LS_RATES_DATA.
              IF SY-SUBRC = 0.
                <LS_RPTCURRENCY> = LS_ENTITY-REG_CURRENCY.
                REGAMOUNT = GBPAMOUNT * LS_RATES_DATA-SIGNEDDATA .
                <LS_FLOW> = 'F_CTA_O'.
                <LS_SIGNEDDATA> = REGAMOUNT - REGAMOUNT_PREV.
                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
              ELSE.
                CONTINUE.
              ENDIF.
            ENDIF.
            ENDIF.

            "ELSEIF WORKING_CATEGORY = 'WFORECAST'. "FLOW IS STILL F_OPE

      WHEN 'F_CTA_O'.
            "FOR CF_ACT_MAIN
            "CASHFLOW WFORECAST AND F_OPE - START
            "QUERY THE RATE TYPE TO USE FOR THIS RECORD VIA RATE TYPE IN THE ASSET CLASS
              CLEAR LS_ACCOUNT.
              READ TABLE LT_ACCOUNT WITH TABLE KEY
              ID = <LS_ACCOUNT>
              INTO LS_ACCOUNT.
              IF SY-SUBRC = 0 .
                "SEE IF LS_ACCOUNT-RATETYPE IS AVG OR CLO
                IF LS_ACCOUNT-RATETYPE IS INITIAL .
                  l_log = 'Check RATETYPE property of ACCOUNT -> ' && ' ' && <LS_ACCOUNT>.
                  cl_ujk_logger=>log( i_object = l_log ).
                  CONTINUE.
                ENDIF.
              ELSE.
                  l_log = 'Check RATETYPE property of ACCOUNT -> ' && ' ' && <LS_ACCOUNT>.
                  cl_ujk_logger=>log( i_object = l_log ).
                  CONTINUE.
              ENDIF.

              "COMPUTE FOR GBP
                IF LS_ENTITY-CURRENCY IS INITIAL.
                CONTINUE.
                ENDIF.
              IF LS_ENTITY-CURRENCY = 'GBP'.
                "WRITE DATA WITHOUT CONVERSION BUT RPTCURRENCY CHANGES TO GBP
                <LS_RPTCURRENCY> = 'GBP'.
                <LS_FLOW> = 'F_OPE_C'.
                <LS_SIGNEDDATA> = ORIGAMOUNT .
                GBPAMOUNT =  <LS_SIGNEDDATA> .
              ELSE.
                "READ RATES TABLE FOR LS_ENTITY-CURRENCY AND LS_ACCOUNT-RATETYPE
                READ TABLE LT_RATES_DATA WITH KEY
                CATEGORY = <LS_CATEGORY>
                INPUTCURRENCY = LS_ENTITY-CURRENCY
                R_ACCOUNT = LS_ACCOUNT-RATETYPE
                TIME = <LS_TIMEID>
                INTO LS_RATES_DATA.
                IF SY-SUBRC = 0.
                  <LS_RPTCURRENCY> = 'GBP'.
                  <LS_FLOW> = 'F_OPE_C'.
                  <LS_SIGNEDDATA> = ORIGAMOUNT / LS_RATES_DATA-SIGNEDDATA .
                  GBPAMOUNT =  <LS_SIGNEDDATA> .
                ELSE.
                  CONTINUE.
                ENDIF.
              ENDIF.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

              "COMPUTE FOR REG_CURRENCY
              IF LS_ENTITY-REG_CURRENCY IS INITIAL.
                CONTINUE.
                ENDIF.

              IF LS_ENTITY-REG_CURRENCY = 'GBP'.
                "NO NEED TO HAVE ANOTHER RECORD OTHERWISE DATA WILL BE DUPLICATED.
              ELSEIF LS_ENTITY-CURRENCY = LS_ENTITY-REG_CURRENCY.
                "WRITE DATA WITHOUT CONVERSION BUT RPTCURRENCY CHANGES TO LS_ENTITY-CURRENCY
                <LS_RPTCURRENCY> = LS_ENTITY-CURRENCY.
                <LS_SIGNEDDATA> = ORIGAMOUNT .
                <LS_FLOW> = 'F_OPE_C'.
                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
              ELSEIF LS_ENTITY-REG_CURRENCY IS INITIAL.
                "DO NOTHING
              ELSE.
                "USE GBP AMOUNT FROM ABOVE
                "GBPAMOUNT.
                "READ RATES TABLE FOR LS_ENTITY-REG_CURRENCY AND LS_ACCOUNT-RATETYPE
                READ TABLE LT_RATES_DATA WITH KEY
                CATEGORY = <LS_CATEGORY>
                INPUTCURRENCY = LS_ENTITY-REG_CURRENCY
                R_ACCOUNT = LS_ACCOUNT-RATETYPE
                TIME = <LS_TIMEID>
                INTO LS_RATES_DATA.
                IF SY-SUBRC = 0.
                  <LS_RPTCURRENCY> = LS_ENTITY-REG_CURRENCY.
                  <LS_SIGNEDDATA> = GBPAMOUNT * LS_RATES_DATA-SIGNEDDATA .
                  <LS_FLOW> = 'F_OPE_C'.
                  COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
                ELSE.
                  CONTINUE.
                ENDIF.
              "ENDIF.
            "CASHFLOW WFORECAST AND F_OPE - END
            ENDIF.
****ADDITION of F99 AS PER EMILYAN'S REQUEST - START
          WHEN 'F99' . "CLOSING BALANCE CONVERT USING CLO RATE


            "5.MAKE GBP CONV CLO RATE ;FLOW = <LS_FLOW>
            IF LS_ENTITY-CURRENCY = 'GBP'.
              "NO NEED TO CONVERT JUST WRITE AS IS BUT CHANGE RPTCURRENCY
              <LS_SIGNEDDATA> = ORIGAMOUNT.
              <LS_RPTCURRENCY> = 'GBP'.
              GBPAMOUNT = <LS_SIGNEDDATA>.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            ELSE.
              CLEAR LS_RATES_DATA.
              READ TABLE LT_RATES_DATA WITH KEY
              CATEGORY = <LS_CATEGORY>
              INPUTCURRENCY = LS_ENTITY-CURRENCY
              R_ACCOUNT = 'CLO'    "'AVG'
              TIME = CURR_PER
              INTO LS_RATES_DATA.
              IF SY-SUBRC = 0.
                <LS_RPTCURRENCY> = 'GBP'.
                <LS_SIGNEDDATA> = ORIGAMOUNT / LS_RATES_DATA-SIGNEDDATA .
                GBPAMOUNT =  <LS_SIGNEDDATA> .
                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
              ELSE.
                CONTINUE.
              ENDIF.
            ENDIF.
            "6.MAKE REG_CURR CLO RATE ;FLOW = <LS_FLOW>
            "APPEND <LS_RESULT_REC> TO <LT_FINAL>.
            IF LS_ENTITY-REG_CURRENCY = 'GBP'.
              "NO NEED TO WRITE TO DATA
              CONTINUE.
            ELSEIF LS_ENTITY-CURRENCY = LS_ENTITY-REG_CURRENCY.
              "NO NEED TO CONVERT JUST CHANGE RPTCURRENCY
              <LS_SIGNEDDATA> = ORIGAMOUNT.
              <LS_RPTCURRENCY> = LS_ENTITY-REG_CURRENCY.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            ELSE.
              "USE THE GBPAMOUNT TO CONVERT TO REG_CURRENCY
              CLEAR LS_RATES_DATA.
              READ TABLE LT_RATES_DATA WITH KEY
               CATEGORY = <LS_CATEGORY>
               INPUTCURRENCY = LS_ENTITY-REG_CURRENCY
               R_ACCOUNT = 'CLO'    "'AVG'
               TIME = CURR_PER
               INTO LS_RATES_DATA.
              IF SY-SUBRC = 0.
                <LS_RPTCURRENCY> = LS_ENTITY-REG_CURRENCY.
                <LS_SIGNEDDATA> = GBPAMOUNT * LS_RATES_DATA-SIGNEDDATA .
                REGAMOUNT = <LS_SIGNEDDATA>.
                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
              ELSE.
                CONTINUE.
              ENDIF.
            ENDIF.

****ADDITION of F99 AS PER EMILYAN'S REQUEST - END
          WHEN OTHERS.

            "CHECK WHETHER IT IS A CHILD OF F_MOVE
            READ TABLE LT_FLOW_F_MOVE
            WITH TABLE KEY TABLE_LINE = <LS_FLOW>
            TRANSPORTING NO FIELDS.
            IF SY-SUBRC <> 0.
              CONTINUE."SKIP THIS RECORD
            ENDIF.
            "CHILDREN OF F_MOVE

            "PROCES FOR FLOW = CHILD OF F_MOVE
            "5.MAKE GBP CONV AVE RATE ;FLOW = <LS_FLOW>
            IF LS_ENTITY-CURRENCY = 'GBP'.
              "NO NEED TO CONVERT JUST WRITE AS IS BUT CHANGE RPTCURRENCY
              <LS_SIGNEDDATA> = ORIGAMOUNT.
              <LS_RPTCURRENCY> = 'GBP'.
              GBPAMOUNT = <LS_SIGNEDDATA>.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            ELSE.
              CLEAR LS_RATES_DATA.
              READ TABLE LT_RATES_DATA WITH KEY
              CATEGORY = <LS_CATEGORY>
              INPUTCURRENCY = LS_ENTITY-CURRENCY
              R_ACCOUNT = 'AVG'
              TIME = CURR_PER
              INTO LS_RATES_DATA.
              IF SY-SUBRC = 0.
                <LS_RPTCURRENCY> = 'GBP'.
                <LS_SIGNEDDATA> = ORIGAMOUNT / LS_RATES_DATA-SIGNEDDATA .
                GBPAMOUNT =  <LS_SIGNEDDATA> .
                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
              ELSE.
                CONTINUE.
              ENDIF.
            ENDIF.
            "6.MAKE REG_CURR AVE RATE ;FLOW = <LS_FLOW>
            "APPEND <LS_RESULT_REC> TO <LT_FINAL>.
            IF LS_ENTITY-REG_CURRENCY = 'GBP'.
              "NO NEED TO WRITE TO DATA
              CONTINUE.
            ELSEIF LS_ENTITY-CURRENCY = LS_ENTITY-REG_CURRENCY.
              "NO NEED TO CONVERT JUST CHANGE RPTCURRENCY
              <LS_SIGNEDDATA> = ORIGAMOUNT.
              <LS_RPTCURRENCY> = LS_ENTITY-REG_CURRENCY.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            ELSE.
              "USE THE GBPAMOUNT TO CONVERT TO REG_CURRENCY
              CLEAR LS_RATES_DATA.
              READ TABLE LT_RATES_DATA WITH KEY
               CATEGORY = <LS_CATEGORY>
               INPUTCURRENCY = LS_ENTITY-REG_CURRENCY
               R_ACCOUNT = 'AVG'
               TIME = CURR_PER
               INTO LS_RATES_DATA.
              IF SY-SUBRC = 0.
                <LS_RPTCURRENCY> = LS_ENTITY-REG_CURRENCY.
                <LS_SIGNEDDATA> = GBPAMOUNT * LS_RATES_DATA-SIGNEDDATA .
                REGAMOUNT = <LS_SIGNEDDATA>.
                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
              ELSE.
                CONTINUE.
              ENDIF.
            ENDIF.

            "PROCES FOR FLOW = CHILD OF F_MOVE
            "7.MAKE GBP CONV CLO - AVE RATE ;COLLECT INTO FLOW F_CTA_M
            "<LS_FLOW> = 'F_CTA_M'.
            " COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            IF LS_ENTITY-CURRENCY = 'GBP'.
              "NO NEED TO CONVERT JUST WRITE AS IS BUT CHANGE RPTCURRENCY AND CLEAR AMOUNT
              CLEAR <LS_SIGNEDDATA>.
              <LS_RPTCURRENCY> = 'GBP'.
              <LS_FLOW> = 'F_CTA_M'.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            ELSE.
              "GBPAMOUNT FROM 5 IS ALREADY AT AVE
              CLEAR LS_RATES_DATA.
              READ TABLE LT_RATES_DATA WITH KEY
              CATEGORY = <LS_CATEGORY>
              INPUTCURRENCY = LS_ENTITY-CURRENCY
              R_ACCOUNT = 'CLO'
              TIME = CURR_PER
              INTO LS_RATES_DATA.
              IF SY-SUBRC = 0.
                <LS_RPTCURRENCY> = 'GBP'.
                <LS_FLOW> = 'F_CTA_M'.
                GBPAMOUNT_CLO = ORIGAMOUNT / LS_RATES_DATA-SIGNEDDATA .
                <LS_SIGNEDDATA> = GBPAMOUNT - GBPAMOUNT_CLO.
                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
              ELSE.
                CONTINUE.
              ENDIF.
            ENDIF.

            "8.MAKE REG_CURR CLO - AVE RATE ;COLLECT INTO FLOW F_CTA_M
            "<LS_FLOW> = 'F_CTA_M'.
            IF LS_ENTITY-REG_CURRENCY = 'GBP'.
              "NO NEED TO WRITE TO DATA
              CONTINUE.
            ELSEIF LS_ENTITY-CURRENCY = LS_ENTITY-REG_CURRENCY.
              "NO NEED TO CONVERT JUST CHANGE RPTCURRENCY
              CLEAR <LS_SIGNEDDATA>.
              <LS_RPTCURRENCY> = LS_ENTITY-REG_CURRENCY.
              <LS_FLOW> = 'F_CTA_M'.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            ELSE.
              "GBAMOUNT IS ALREADY KNOW AND IT IS AVE
              "REGAMOUNT IS KNOWN AND IT IS AVE
              "GBAMOUNT_CLO IS ALEADY KNOWN FROM 7
              "CALCULATE REGAMOUNT_CLO
              CLEAR LS_RATES_DATA.
              READ TABLE LT_RATES_DATA WITH KEY
               CATEGORY = <LS_CATEGORY>
               INPUTCURRENCY = LS_ENTITY-REG_CURRENCY
               R_ACCOUNT = 'CLO'
               TIME = CURR_PER
               INTO LS_RATES_DATA.
              IF SY-SUBRC = 0.
                <LS_RPTCURRENCY> = LS_ENTITY-REG_CURRENCY.
                REGAMOUNT_CLO = GBPAMOUNT_CLO * LS_RATES_DATA-SIGNEDDATA .
                <LS_SIGNEDDATA> = REGAMOUNT_CLO - REGAMOUNT.
                <LS_FLOW> = 'F_CTA_M'.
                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
              ELSE.
                CONTINUE.
              ENDIF.
            ENDIF.
        ENDCASE.

      ENDLOOP.
      CLEAR CT_DATA.
      APPEND LINES OF <LT_FINAL> TO CT_DATA.

    ELSEIF FLAG = 'R'. "RESOURCE DRIVER CALCULATION I.E. 420 430
      "USED BY ITSS RESOURCE MODEL
***********PROCESS THE CT_DATA*****************
      SORT CT_DATA.
      CLEAR <LT_FINAL>.

      LOOP AT CT_DATA ASSIGNING <LS_REC>.
        CLEAR:ORIGAMOUNT, GBPAMOUNT  , GBPAMOUNT_PREV  ,  GBPAMOUNT_CLO   ,
          REGAMOUNT       ,REGAMOUNT_PREV  ,  REGAMOUNT_CLO   ,      PREV_PER        ,
          CURR_PER        ,  YEAR        ,     PERIOD        .

        <LS_RESULT_REC> = <LS_REC>.

        ASSIGN COMPONENT 'RESOURCE_ACCT'  OF STRUCTURE <LS_RESULT_REC> TO <LS_ACCOUNT>.
        ASSIGN COMPONENT 'ENTITY'         OF STRUCTURE <LS_RESULT_REC> TO <LS_ENTITY>.
        ASSIGN COMPONENT 'CATEGORY'       OF STRUCTURE <LS_RESULT_REC> TO <LS_CATEGORY>.
        ASSIGN COMPONENT 'TIME'           OF STRUCTURE <LS_RESULT_REC> TO <LS_TIMEID>.
        ASSIGN COMPONENT 'CURRENCY'       OF STRUCTURE <LS_RESULT_REC> TO <CURRENCY>.
        ASSIGN COMPONENT 'RATETYPE'       OF STRUCTURE <LS_RESULT_REC> TO <RATETYPE>.
        ASSIGN COMPONENT 'INPUTGBP'       OF STRUCTURE <LS_RESULT_REC> TO <INPUTGBP>.
        ASSIGN COMPONENT 'OUTPUTLC'       OF STRUCTURE <LS_RESULT_REC> TO <OUTPUTLC>.
        ORIGAMOUNT =  <INPUTGBP>.

        "ONLY CONVERT RECORDS TIME IS PROPER
        IF <LS_TIMEID>+5(2) = 'IN'. "ITS IS GIVING IN YEAR_INP
          APPEND <LS_RESULT_REC> TO <LT_FINAL>.
          ENDIF.

        IF <CURRENCY> IS INITIAL.
          APPEND <LS_RESULT_REC> TO <LT_FINAL>.
          ENDIF.

        IF <RATETYPE> IS INITIAL.
          APPEND <LS_RESULT_REC> TO <LT_FINAL>.
          ENDIF.

        IF <INPUTGBP> IS INITIAL OR <INPUTGBP> = 0.
          APPEND <LS_RESULT_REC> TO <LT_FINAL>.
          ENDIF.

        "INPUT IS GBP -- CONVERT TO CURRENCY INDICATED BY <CURRENCY>.
        "RATE_TYPE WILL BE AVG OR READ FROM <INPUTGBP>

        "COMPUTE FOR LC
          "READ RATES TABLE FOR LS_ENTITY-CURRENCY AND LS_ACCOUNT-RATETYPE
          READ TABLE LT_RATES_DATA WITH KEY
          CATEGORY      = <LS_CATEGORY>
          INPUTCURRENCY = <CURRENCY>
          R_ACCOUNT     = <RATETYPE>
          TIME          = <LS_TIMEID>
          INTO LS_RATES_DATA.

          IF SY-SUBRC = 0 AND LS_RATES_DATA-SIGNEDDATA <> 0.
            <OUTPUTLC> = ORIGAMOUNT * LS_RATES_DATA-SIGNEDDATA .
          ELSE.
            <OUTPUTLC> = 0.
          ENDIF.

          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

      ENDLOOP.



      CLEAR CT_DATA.
      APPEND LINES OF <LT_FINAL> TO CT_DATA.

    ELSE.
      "DO NON CASHFLOW CALC FX HERE
***********PROCESS THE CT_DATA*****************
      SORT CT_DATA.
      CLEAR <LT_FINAL>.

      LOOP AT CT_DATA ASSIGNING <LS_REC>.
        CLEAR:ORIGAMOUNT, GBPAMOUNT  , GBPAMOUNT_PREV  ,  GBPAMOUNT_CLO   ,
          REGAMOUNT       ,REGAMOUNT_PREV  ,  REGAMOUNT_CLO   ,      PREV_PER        ,
          CURR_PER        ,  YEAR        ,     PERIOD        .

        <LS_RESULT_REC> = <LS_REC>.
        ASSIGN COMPONENT ACCOUNT_DIM    OF STRUCTURE <LS_RESULT_REC> TO <LS_ACCOUNT>.
        ASSIGN COMPONENT ENTITY_DIM     OF STRUCTURE <LS_RESULT_REC> TO <LS_ENTITY>.

        ASSIGN COMPONENT 'CATEGORY'     OF STRUCTURE <LS_RESULT_REC> TO <LS_CATEGORY>.
        ASSIGN COMPONENT 'TIME'         OF STRUCTURE <LS_RESULT_REC> TO <LS_TIMEID>.
        ASSIGN COMPONENT 'RPTCURRENCY'  OF STRUCTURE <LS_RESULT_REC> TO <LS_RPTCURRENCY>.
        ASSIGN COMPONENT 'SIGNEDDATA'   OF STRUCTURE <LS_RESULT_REC> TO <LS_SIGNEDDATA>.
        ORIGAMOUNT =  <LS_SIGNEDDATA>.

        "ONLY CONVERT RECORDS IF THEY ARE LC
        IF <LS_RPTCURRENCY> <> 'LC'.
          CONTINUE.
        ENDIF.

        "ONLY CONVERT RECORDS TIME IS PROPER
        IF <LS_TIMEID>+5(2) = 'IN'.
          CONTINUE.
        ENDIF.

        "FOR EVERY RECORD CONVERT LC ENTRY INTO GBP AND REGCURRENCY

        "QUERY THE CURRENCY AND REG_CURRENCY OF THE ENTITY
        CLEAR LS_ENTITY.
        READ TABLE LT_ENTITY WITH TABLE KEY
        ID = <LS_ENTITY>
        INTO LS_ENTITY.
        IF SY-SUBRC = 0 .
          "CHECK IF CURRENCY & REG_CURRENCY ARE NON BLANK
          IF LS_ENTITY-CURRENCY IS INITIAL OR
            LS_ENTITY-REG_CURRENCY IS INITIAL.
            l_log = 'Check Currency or reg_currency property of ' && ' ' && <LS_ENTITY>.
            cl_ujk_logger=>log( i_object = l_log ).
            CONTINUE.
          ENDIF.
        ELSE.
            l_log = 'Check Currency or reg_currency property of ' && ' ' && <LS_ENTITY>.
            cl_ujk_logger=>log( i_object = l_log ).
            CONTINUE.
        ENDIF.

        "QUERY THE RATE TYPE TO USE FOR THIS RECORD VIA RATE TYPE IN THE ASSET CLASS
        CLEAR LS_ACCOUNT.
        READ TABLE LT_ACCOUNT WITH TABLE KEY
        ID = <LS_ACCOUNT>
        INTO LS_ACCOUNT.
        IF SY-SUBRC = 0 .
          "SEE IF LS_ACCOUNT-RATETYPE IS AVG OR CLO
          IF LS_ACCOUNT-RATETYPE IS INITIAL .
            l_log = 'Check RATETYPE property of ACCOUNT -> ' && ' ' && <LS_ACCOUNT>.
            cl_ujk_logger=>log( i_object = l_log ).
            CONTINUE.
          ENDIF.
        ELSE.
            l_log = 'Check RATETYPE property of ACCOUNT -> ' && ' ' && <LS_ACCOUNT>.
            cl_ujk_logger=>log( i_object = l_log ).
            CONTINUE.
        ENDIF.

        "COMPUTE FOR GBP
          IF LS_ENTITY-CURRENCY IS INITIAL.
          CONTINUE.
          ENDIF.
        IF LS_ENTITY-CURRENCY = 'GBP'.
          "WRITE DATA WITHOUT CONVERSION BUT RPTCURRENCY CHANGES TO GBP
          <LS_RPTCURRENCY> = 'GBP'.
          <LS_SIGNEDDATA> = ORIGAMOUNT .
          GBPAMOUNT =  <LS_SIGNEDDATA> .
        ELSE.
          "READ RATES TABLE FOR LS_ENTITY-CURRENCY AND LS_ACCOUNT-RATETYPE
          READ TABLE LT_RATES_DATA WITH KEY
          CATEGORY = <LS_CATEGORY>
          INPUTCURRENCY = LS_ENTITY-CURRENCY
          R_ACCOUNT = LS_ACCOUNT-RATETYPE
          TIME = <LS_TIMEID>
          INTO LS_RATES_DATA.
          IF SY-SUBRC = 0.
            <LS_RPTCURRENCY> = 'GBP'.
            <LS_SIGNEDDATA> = ORIGAMOUNT / LS_RATES_DATA-SIGNEDDATA .
            GBPAMOUNT =  <LS_SIGNEDDATA> .
          ELSE.
            CONTINUE.
          ENDIF.
        ENDIF.
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

        "COMPUTE FOR REG_CURRENCY
        IF LS_ENTITY-REG_CURRENCY IS INITIAL.
          CONTINUE.
          ENDIF.

        IF LS_ENTITY-REG_CURRENCY = 'GBP'.
          "NO NEED TO HAVE ANOTHER RECORD OTHERWISE DATA WILL BE DUPLICATED.
        ELSEIF LS_ENTITY-CURRENCY = LS_ENTITY-REG_CURRENCY.
          "WRITE DATA WITHOUT CONVERSION BUT RPTCURRENCY CHANGES TO LS_ENTITY-CURRENCY
          <LS_RPTCURRENCY> = LS_ENTITY-CURRENCY.
          <LS_SIGNEDDATA> = ORIGAMOUNT .
          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
        ELSEIF LS_ENTITY-REG_CURRENCY IS INITIAL. "RDG 2017-FEB-06 maybe to fix duplicate record
          "DO NOTHING
        ELSE.
          "USE GBP AMOUNT FROM ABOVE
          "GBPAMOUNT.
          "READ RATES TABLE FOR LS_ENTITY-REG_CURRENCY AND LS_ACCOUNT-RATETYPE
          READ TABLE LT_RATES_DATA WITH KEY
          CATEGORY = <LS_CATEGORY>
          INPUTCURRENCY = LS_ENTITY-REG_CURRENCY
          R_ACCOUNT = LS_ACCOUNT-RATETYPE
          TIME = <LS_TIMEID>
          INTO LS_RATES_DATA.
          IF SY-SUBRC = 0.
            <LS_RPTCURRENCY> = LS_ENTITY-REG_CURRENCY.
            <LS_SIGNEDDATA> = GBPAMOUNT * LS_RATES_DATA-SIGNEDDATA .
            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
          ELSE.
            CONTINUE.
          ENDIF.
        ENDIF.

      ENDLOOP.

      CLEAR CT_DATA.
      APPEND LINES OF <LT_FINAL> TO CT_DATA.

    ENDIF. "End APPL ID Check for CF

  ENDMETHOD.


  METHOD IF_UJ_CUSTOM_LOGIC~CLEANUP.
  ENDMETHOD.


  METHOD IF_UJ_CUSTOM_LOGIC~EXECUTE.
*&---------------------------------------------------------------------*
*&  Method           IF_UJ_CUSTOM_LOGIC~EXECUTE
*&---------------------------------------------------------------------*
****************************************************************************
* Program type....:  Customer SO Custom Method
* Author...........:
* Date.............: February 2017
* ----------------------------------------------------------------------
* Description......: Main routing method of Customer SO_PLANNING solution
*                    used to route calls between methods and read initial
*                    parameters from script logic - not allowing passing
*                    multiple CATEGORIES simultaneously, for example.
*
****************************************************************************
* Change Log
****************************************************************************
* Changed on:                 By:
* Description:
****************************************************************************
INCLUDE ZINC_PLAN_BADI_COMMON_VARS. "Common Variables

**** CHECK THE CORRECT RUNNING ENVIRONMENT ***********

IF ZCL_TVARVC=>GET_SINGLE_VALUE( 'BPC_PLAN_ENV' ) IS INITIAL.
      MESSAGE e010(ZCL_BPC_PLANNING_MSG) INTO L_LOG WITH 'BPC_PLAN_ENV'.
      CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).
      CLEAR CT_DATA.
      RAISE EXCEPTION TYPE CX_UJ_CUSTOM_LOGIC.
ENDIF.

*Removed for purposes of using multiple training environments
*IF I_APPSET_ID <> ZCL_TVARVC=>GET_SINGLE_VALUE( 'BPC_PLAN_ENV' ). "Pick up the relevant environment
*         MESSAGE i002(ZCL_BPC_PLANNING_MSG) INTO L_LOG.
*         CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).
*         MESSAGE i009(ZCL_BPC_PLANNING_MSG) INTO L_LOG
*         WITH ZCL_TVARVC=>GET_SINGLE_VALUE( 'BPC_PLAN_ENV' ).
*         CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).
*         MESSAGE i003(ZCL_BPC_PLANNING_MSG) INTO L_LOG.
*         CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).
*      CLEAR CT_DATA.
*      RAISE EXCEPTION TYPE CX_UJ_CUSTOM_LOGIC.
*ENDIF.

***************** READ PACKAGE PARAMETERS ********************
*** Used to pass the parameter from script logic to BADI
*** Parameters control the route within the class' methods
**************************************************************

DATA: SUB_ID           TYPE CHAR05, "id of the required sub-routine per calculations register
      WORKING_CATEGORY TYPE STRING.
******************************************************
****** GET PARAMETERS ********************************
    CLEAR: LS_PARAM, ET_MESSAGE.
    READ TABLE IT_PARAM WITH KEY HASHKEY = CALLER_ID_PARAM INTO LS_PARAM.
    CALLER_ID = LS_PARAM-HASHVALUE.
    IF SY-SUBRC <> 0.
         MESSAGE i002(ZCL_BPC_PLANNING_MSG) INTO L_LOG.
         CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).
         MESSAGE i000(ZCL_BPC_PLANNING_MSG) INTO L_LOG WITH CALLER_ID_PARAM.
         CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).
         MESSAGE i003(ZCL_BPC_PLANNING_MSG) INTO L_LOG.
         CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).
      EXIT.
    ENDIF.

    READ TABLE IT_PARAM WITH KEY HASHKEY = SUB_ID_PARAM INTO LS_PARAM.
    SUB_ID = LS_PARAM-HASHVALUE.
    IF SY-SUBRC <> 0.
         MESSAGE i002(ZCL_BPC_PLANNING_MSG) INTO L_LOG.
         CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).
         MESSAGE i000(ZCL_BPC_PLANNING_MSG) INTO L_LOG WITH SUB_ID_PARAM.
         CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).
         MESSAGE i003(ZCL_BPC_PLANNING_MSG) INTO L_LOG.
         CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).

      RAISE EXCEPTION TYPE CX_UJ_CUSTOM_LOGIC.
      EXIT.
    ENDIF.

*** Pick Category
    READ TABLE IT_CV INTO CV_CATEGORY WITH TABLE KEY DIM_UPPER_CASE = 'CATEGORY'.
**** Cant process two categoroes simultaneously due to time dim restrictions in WFORECAST and BUDGET
    LOOP AT CV_CATEGORY-MEMBER INTO WORKING_CATEGORY.
      IF SY-TABIX > 1.
        CLEAR: ET_MESSAGE, L_LOG.
         MESSAGE i002(ZCL_BPC_PLANNING_MSG) INTO L_LOG.
         CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).
         MESSAGE e001(ZCL_BPC_PLANNING_MSG) INTO L_LOG.
         CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).
         MESSAGE i003(ZCL_BPC_PLANNING_MSG) INTO L_LOG.
         CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).
        EXIT.
      ELSE.
      ENDIF.

    ENDLOOP.
************
    IF L_LOG IS NOT INITIAL.
      EXIT.
    ENDIF.



    CASE CALLER_ID. "Cases grouped by params passed

     WHEN 'CLEAR' "Clear Forecast on previous period
       OR 'IS_ACT_MAIN' "Income Statement Actualisation
       OR 'CF_ACT_MAIN' "Cashflow main actualisation method
       OR 'CASHFLOW' "Cashflow model default calculations
       OR 'INC_STATEMENT'. "Income statement model default calculations

        TRY.
            CALL METHOD (CALLER_ID)
              EXPORTING
                I_APPSET_ID      = I_APPSET_ID
                I_APPL_ID        = I_APPL_ID
                SUB_ID           = SUB_ID
                WORKING_CATEGORY = WORKING_CATEGORY
                IT_CV            = IT_CV
              IMPORTING
                ET_MESSAGE       = ET_MESSAGE
              CHANGING
                CT_DATA          = CT_DATA.

          CATCH CX_UJ_CUSTOM_LOGIC.
            EXIT.
        ENDTRY.

       WHEN 'TRANSFER_IS_BS' "Transfer data from Income Statement to CashFlow model
         OR 'TRANSFER_CP_IS' "Transfer data from Capex to Income Statement
         OR 'CAPEX_NEW'. "Capex reworked method

         "OR 'TRANSFER_RS_IS'."Transfer data from Resource to Income Statement
.
        TRY.

            CALL METHOD (CALLER_ID)
              EXPORTING
                I_APPSET_ID      = I_APPSET_ID
                I_APPL_ID        = I_APPL_ID
                IT_CV            = IT_CV
                IT_PARAM         = IT_PARAM
                WORKING_CATEGORY = WORKING_CATEGORY
              IMPORTING
                ET_MESSAGE       = ET_MESSAGE
              CHANGING
                CT_DATA          = CT_DATA.

          CATCH CX_UJ_CUSTOM_LOGIC .
            EXIT.
        ENDTRY.

  WHEN 'CLEAR_INACTIVE'. "Clear inactive entity members from forecast


        TRY.
            CALL METHOD (CALLER_ID)
              EXPORTING
                I_APPSET_ID      = I_APPSET_ID
                I_APPL_ID        = I_APPL_ID
                IT_PARAM         = IT_PARAM
                SUB_ID           = SUB_ID
                WORKING_CATEGORY = WORKING_CATEGORY
                IT_CV            = IT_CV
              IMPORTING
                ET_MESSAGE       = ET_MESSAGE
              CHANGING
                CT_DATA          = CT_DATA.

          CATCH CX_UJ_CUSTOM_LOGIC.
            EXIT.
        ENDTRY.


     WHEN 'CREATE_SNAPSHOT'. "Clear existing snapshot and copy from WORKING_CATEGORY

        TRY.
            CALL METHOD (CALLER_ID)
              EXPORTING
                I_APPSET_ID      = I_APPSET_ID
                I_APPL_ID        = I_APPL_ID
                SUB_ID           = SUB_ID
                WORKING_CATEGORY = 'WFORECAST'
                IT_CV            = IT_CV
              IMPORTING
                ET_MESSAGE       = ET_MESSAGE
              CHANGING
                CT_DATA          = CT_DATA.

          CATCH CX_UJ_CUSTOM_LOGIC.
            EXIT.
        ENDTRY.


      WHEN 'FXTRANS'. "Currency Translation

        TRY.
            CALL METHOD FXTRANS
              EXPORTING
                I_APPSET_ID = I_APPSET_ID
                I_APPL_ID   = I_APPL_ID
                IT_CV       = IT_CV
              IMPORTING
                ET_MESSAGE  = ET_MESSAGE
              CHANGING
                CT_DATA     = CT_DATA.

          CATCH CX_UJ_CUSTOM_LOGIC .
            EXIT.
        ENDTRY.

      WHEN 'CAPEX'. "CAPEX CALC

        TRY.
            CALL METHOD CAPEX
              EXPORTING
                I_APPSET_ID = I_APPSET_ID
                I_APPL_ID   = I_APPL_ID
                IT_CV       = IT_CV
              IMPORTING
                ET_MESSAGE  = ET_MESSAGE
              CHANGING
                CT_DATA     = CT_DATA.

          CATCH CX_UJ_CUSTOM_LOGIC .
            EXIT.
        ENDTRY.

      WHEN 'RESOURCE'. "Resource Calculations

        TRY.
            CALL METHOD RESOURCE_CALC
              EXPORTING
                I_APPSET_ID = I_APPSET_ID
                I_APPL_ID   = I_APPL_ID
                IT_CV       = IT_CV
                SUB_ID      = SUB_ID
                IT_PARAM    = IT_PARAM
              IMPORTING
                ET_MESSAGE  = ET_MESSAGE
              CHANGING
                CT_DATA     = CT_DATA.

          CATCH CX_UJ_CUSTOM_LOGIC .
            EXIT.
        ENDTRY.

      WHEN 'TRANSFER_RS_IS'."Transfer data from Resource to Income Statement
        TRY.
          CALL METHOD me->TRANSFER_RS_IS
            EXPORTING
              I_APPSET_ID      = I_APPSET_ID
              I_APPL_ID        = I_APPL_ID
              IT_CV            = IT_CV
              IT_PARAM         = IT_PARAM
              WORKING_CATEGORY = WORKING_CATEGORY
              SUB_ID           = SUB_ID
            IMPORTING
              ET_MESSAGE       = ET_MESSAGE
            CHANGING
              CT_DATA          = CT_DATA.
              .
           CATCH CX_UJ_CUSTOM_LOGIC .
          ENDTRY.

       WHEN OTHERS.
        CLEAR: ET_MESSAGE, L_LOG.
        L_LOG = 'CALLER_ID is not recognised'.
        CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).


    ENDCASE.

  ENDMETHOD.


  METHOD IF_UJ_CUSTOM_LOGIC~INIT.
  ENDMETHOD.


  METHOD INC_STATEMENT.
*&---------------------------------------------------------------------*
*&  Method           INC_STATEMENT
*&---------------------------------------------------------------------*
****************************************************************************
* Program type....:  Customer SO Custom Method
* Author...........:   ()
* Date.............: February 2017
* --------------------------------------------------------------------------
* Description......: This method handles all the relevant calculations
*                    within the INC_STATEMENT model
*
*
****************************************************************************
* Change Log
*           1. Signage on AC213000 salestax should be positive,
*              on AC310000 Salestax should be negative.
*           2. Change per defect 2230 - duplicate errors in log
*           3. Do not calculate for CAPEX_INPUT and RESOURCE_INPUT audittrails - defect CMC002
*           4. Additional changes per defect 2230. Were losing zeroes.
*           5. Multiple categories being read. Defect 2787 resolution.
*           6. 27072017 - Do not calculate RESOURCE_ITSCREDIT also.
*           7. WO0000002040082 - we assign VAR_AUDITTRAIL to BW_MW_TAX ,
*              therefore VAR_AUDITTRAIL is never BW_MW in the following lines
*              where we need to decide what account we need to assign.
****************************************************************************
* Changed on:    31.07.2017             By: RDG
* Description:
****************************************************************************

    INCLUDE ZINC_PLAN_BADI_COMMON_VARS. "Common Variables

*** SET TOOLS INSTANCE *****
    CREATE OBJECT LO_TOOLS
      EXPORTING
        I_ENV_ID = I_APPSET_ID
        I_MOD_ID = I_APPL_ID.

************************ASSIGN STRUCTURE OF INCOMING MODEL ****
    LO_TOOLS->WRITE_MODEL_DATA(
      EXPORTING
       TARGET_MODEL = I_APPL_ID "self structure
        WRITE       = 'N'
     IMPORTING
       LR_DATA     = LR_DATA ).
    ASSIGN LR_DATA->* TO <CT_DATA>.
**********************************************************************
************ ASSIGN WAs **********************************************
    CREATE DATA LT_FINAL LIKE CT_DATA.
    ASSIGN LT_FINAL->* TO <LT_FINAL>.
    CREATE DATA LS_RESULT_REC LIKE LINE OF CT_DATA.
    ASSIGN LS_RESULT_REC->* TO <LS_RESULT_REC>.
    CREATE DATA LS_REC LIKE LINE OF CT_DATA.
    ASSIGN LS_REC->* TO <LS_REC>.
***************************************************************************


*****************************************************************************
*****************************************************************************

****** GET TIMESCOPE AND VARIABLES
    LO_TOOLS->TIME(
       EXPORTING
         CATEGORY    = WORKING_CATEGORY
       IMPORTING
        TIMEQRTLY   = LT_TIMEQRTLY
        TIMESCOPE   = LT_TIMESCOPE
        FIRST_PERIOD   = FIRST_PERIOD
        LAST_PERIOD = LAST_PERIOD
        CURRENT_PERIOD = CURRENT_PERIOD ).

****** END GET TIMESCOPE AND VARIABLES

******* GET ALL ANALYSIS ********************************

    ANALYSIS = LO_TOOLS->GET_PARENT_MD(
    EXPORTING
    PARENT_MBR = 'ALL_ANALYSIS'
    I_DIMENSION_ID = 'ANALYSIS').


    READ TABLE IT_CV INTO CV_AUDITTRAIL WITH TABLE KEY DIM_UPPER_CASE = 'AUDITTRAIL'.

******************** SUBS START **************************************
**********************************************************************
**********************************************************************
**********************************************************************
**********************************************************************
**********************************************************************
**********************************************************************
**********************************************************************
**********************************************************************
**********************************************************************

    CASE SUB_ID.

      WHEN 'SUB3'.

***** GET ACCOUNTS WHERE DIMLIST = ACR
        CLEAR: LT_SEL, LS_SEL.

        LS_SEL-DIMENSION = 'ACCOUNT'.
        LS_SEL-ATTRIBUTE = 'DIMLIST'.
        LS_SEL-SIGN = 'I'.
        LS_SEL-OPTION = 'EQ'.
        LS_SEL-LOW = 'ACR'.
        APPEND LS_SEL TO LT_SEL.

        LO_TOOLS->READ_MASTER_DATA(
          EXPORTING
            I_DIMENSION_ID   = 'ACCOUNT'
          IMPORTING
            OUTPUT_R_DATA = MASTER_DATA
          CHANGING
            LT_SEL        = LT_SEL ).

        ASSIGN MASTER_DATA->* TO <DIMENSION_DATA>.
        MOVE-CORRESPONDING <DIMENSION_DATA> TO LT_ACCOUNT.


**************************************************************
*** PREPARE READ FROM MODEL
***** GET ENTITY FROM IT_CV **********************************

        READ TABLE IT_CV INTO CV_ENTITY WITH TABLE KEY DIM_UPPER_CASE = 'ENTITY'.


        CLEAR: LT_SEL, LS_SEL.

        LS_SEL-ATTRIBUTE = 'ID'.
        LS_SEL-SIGN = 'I'.
        LS_SEL-OPTION = 'EQ'.

        LOOP AT CV_ENTITY-MEMBER INTO WA_MEMBER.

          LS_SEL-DIMENSION = 'ENTITY'.
          LS_SEL-LOW = WA_MEMBER.
          APPEND LS_SEL TO LT_SEL.
          APPEND LS_SEL TO LT_DRIVER_SEL. "FOR DRIVERS SELECTION
        ENDLOOP.

        LOOP AT LT_ACCOUNT INTO LS_ACCOUNT.
          LS_SEL-DIMENSION = 'ACCOUNT'.
          LS_SEL-LOW = LS_ACCOUNT-ID.
          APPEND LS_SEL TO LT_SEL.
          APPEND LS_SEL TO LT_DRIVER_SEL. "FOR DRIVERS SELECTION
        ENDLOOP.

**** add additional accounts for zeroing out *************************
        LS_SEL-LOW = 'AC210030'.
        APPEND LS_SEL TO LT_SEL.
        LS_SEL-LOW = 'AC210000'.
        APPEND LS_SEL TO LT_SEL.
        LS_SEL-LOW = 'AC310000'.
        APPEND LS_SEL TO LT_SEL.
**********************************************************************

*        LOOP AT CV_CATEGORY-MEMBER INTO WA_MEMBER.
*        Change 5 implemented
          LS_SEL-DIMENSION = 'CATEGORY'.
          LS_SEL-LOW = WORKING_CATEGORY.
*          LS_SEL-LOW = WA_MEMBER.
          APPEND LS_SEL TO LT_SEL.
          APPEND LS_SEL TO LT_DRIVER_SEL. "FOR DRIVERS SELECTION
*        ENDLOOP.


        CLEAR COUNTER.

        LOOP AT CV_AUDITTRAIL-MEMBER INTO WA_MEMBER.

          LS_SEL-DIMENSION = 'AUDITTRAIL'.
          LS_SEL-ATTRIBUTE = 'ID'.
          LS_SEL-SIGN = 'I'.
          LS_SEL-OPTION = 'EQ'.
          LS_SEL-LOW = WA_MEMBER.

          "***********defect CMC002 change start *****************
          IF WA_MEMBER = 'CAPEX_INPUT'
            OR WA_MEMBER = 'RESOURCE_INPUT' ". Change 6 remove
            OR WA_MEMBER = 'RESOURCE_ITSCREDIT'. "Change 6 add
           WA_MEMBER = 'EXIT'.
           EXIT.
           ELSE.
            APPEND LS_SEL TO LT_SEL.
           ENDIF.
          "***********defect CMC002 change end *******************

**** add additional audittrail to read data if drivers are being changed

          IF ( WA_MEMBER = 'DR_BILLOFFSET'
                         OR WA_MEMBER = 'DR_CASHOFFSET'
                         OR WA_MEMBER = 'DR_INDIRECTTAX' ) AND COUNTER = 0.
            LS_SEL-LOW = 'INPUT'.
            APPEND LS_SEL TO LT_SEL.
            COUNTER = 1.
          ENDIF.
*************************************************************************

        ENDLOOP.

        IF WA_MEMBER = 'EXIT'.
          EXIT.
        ENDIF.

        CLEAR: LS_SEL, WA_MEMBER.

        LOOP AT ANALYSIS INTO WA_MEMBER.

          LS_SEL-DIMENSION = 'ANALYSIS'.
          LS_SEL-ATTRIBUTE = 'ID'.
          LS_SEL-SIGN = 'I'.
          LS_SEL-OPTION = 'EQ'.
          LS_SEL-LOW = WA_MEMBER.
          APPEND LS_SEL TO LT_SEL.
          APPEND LS_SEL TO LT_DRIVER_SEL. "FOR DRIVERS SELECTION

        ENDLOOP.
**** add additional analysis to zero out *******************************
        LS_SEL-LOW = 'F_UNBILLED'.
        APPEND LS_SEL TO LT_SEL.
        LS_SEL-LOW = 'F_TRFBILLDEBT'.
        APPEND LS_SEL TO LT_SEL.
        LS_SEL-LOW = 'F_BILLED'.
        APPEND LS_SEL TO LT_SEL.
        LS_SEL-LOW = 'F_SALESTAXCR'.
        APPEND LS_SEL TO LT_SEL.
        LS_SEL-LOW = 'F_CASH_CALC'.
        APPEND LS_SEL TO LT_SEL.
************************************************************************

        CLEAR: LS_SEL.

        LS_SEL-DIMENSION = 'PARTNER'.
        LS_SEL-ATTRIBUTE = 'ID'.
        LS_SEL-SIGN = 'I'.
        LS_SEL-OPTION = 'EQ'.
        LS_SEL-LOW = 'I_NONE'.
        APPEND LS_SEL TO LT_SEL.

        CLEAR: LS_SEL, WA_MEMBER, LS_TIME.

        LOOP AT LT_TIMESCOPE INTO LS_TIME.

          LS_SEL-DIMENSION = 'TIME'.
          LS_SEL-ATTRIBUTE = 'ID'.
          LS_SEL-SIGN = 'I'.
          LS_SEL-OPTION = 'EQ'.
          LS_SEL-LOW = LS_TIME-ID.
          APPEND LS_SEL TO LT_SEL.

        ENDLOOP.


        SORT LT_SEL BY LOW.
        DELETE ADJACENT DUPLICATES FROM LT_SEL.

        LO_TOOLS->READ_MODEL_DATA(
          EXPORTING
            IT_SEL      = LT_SEL
          IMPORTING
            OUTPUT_DATA = <CT_DATA> ).

        CLEAR CT_DATA.
        MOVE-CORRESPONDING <CT_DATA> TO CT_DATA.

***************************************  READ RELEVANT DRIVERS ***********************************

        CLEAR: LS_SEL, <CT_DATA>.

        LS_SEL-DIMENSION = 'TIME'.
        LS_SEL-ATTRIBUTE = 'ID'.
        LS_SEL-SIGN = 'I'.
        LS_SEL-OPTION = 'EQ'.
        LS_SEL-LOW = 'XXXX.INP'.
        APPEND LS_SEL TO LT_DRIVER_SEL.

        LS_SEL-DIMENSION = 'AUDITTRAIL'.
        LS_SEL-ATTRIBUTE = 'ID'.
        LS_SEL-SIGN = 'I'.
        LS_SEL-OPTION = 'EQ'.
        LS_SEL-LOW = 'DR_BILLOFFSET'.
        APPEND LS_SEL TO LT_DRIVER_SEL.
        LS_SEL-LOW = 'DR_CASHOFFSET'.
        APPEND LS_SEL TO LT_DRIVER_SEL.
        LS_SEL-LOW = 'DR_INDIRECTTAX'.
        APPEND LS_SEL TO LT_DRIVER_SEL.

        LO_TOOLS->READ_MODEL_DATA(
          EXPORTING
            IT_SEL      = LT_DRIVER_SEL
          IMPORTING
            OUTPUT_DATA = <CT_DATA> ).

        MOVE-CORRESPONDING <CT_DATA> TO LT_DRIVERS. "<----- Reference LT_DRIVERS table for all your driving needs!

***************************************************************************************************
*** WRITE ZEROES *******


        LOOP AT CT_DATA ASSIGNING <LS_RESULT_REC>.
          ASSIGN COMPONENT 'ACCOUNT' OF STRUCTURE <LS_RESULT_REC> TO <LS_ACCOUNT>.
          ASSIGN COMPONENT 'ANALYSIS' OF STRUCTURE <LS_RESULT_REC> TO <LS_ANALYSIS>.
          ASSIGN COMPONENT 'SIGNEDDATA' OF STRUCTURE <LS_RESULT_REC> TO <LS_SIGNEDDATA>.

          IF <LS_ACCOUNT> = 'AC210000'
             AND ( <LS_ANALYSIS> = 'F_BILLED' OR <LS_ANALYSIS> = 'F_CASH_CALC' OR <LS_ANALYSIS> = 'F_SALESTAXCR' ).
            <LS_SIGNEDDATA> = 0.
            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
          ELSEIF <LS_ACCOUNT> = 'AC210030'
            AND ( <LS_ANALYSIS> = 'F_UNBILLED' OR <LS_ANALYSIS> = 'F_TRFBILLDEBT').
            <LS_SIGNEDDATA> = 0.
            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
          ENDIF.
          IF <LS_ACCOUNT> = 'AC310000'
            AND <LS_ANALYSIS> = 'F_SALESTAXCR'.
            <LS_SIGNEDDATA> = 0.
            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
          ENDIF.

        ENDLOOP.

        CREATE DATA LS_RESULT_REC LIKE LINE OF CT_DATA.
        ASSIGN LS_RESULT_REC->* TO <LS_RESULT_REC>.

        LOOP AT CT_DATA ASSIGNING <LS_REC>.
          <LS_RESULT_REC> = <LS_REC>.
          ASSIGN COMPONENT 'ACCOUNT' OF STRUCTURE <LS_RESULT_REC> TO <LS_ACCOUNT>.
          ASSIGN COMPONENT 'ANALYSIS' OF STRUCTURE <LS_RESULT_REC> TO <LS_ANALYSIS>.
          ASSIGN COMPONENT 'ENTITY' OF STRUCTURE <LS_RESULT_REC> TO <LS_ENTITY>.
          ASSIGN COMPONENT 'TIME' OF STRUCTURE <LS_RESULT_REC> TO <LS_TIME>.
          ASSIGN COMPONENT 'CATEGORY' OF STRUCTURE <LS_RESULT_REC> TO <LS_CATEGORY>.
          ASSIGN COMPONENT 'SIGNEDDATA' OF STRUCTURE <LS_RESULT_REC> TO <LS_SIGNEDDATA>.

          READ TABLE LT_ACCOUNT WITH KEY ID = <LS_ACCOUNT> TRANSPORTING NO FIELDS.
****** SPECIAL CALCULATIONS FOR ACR ACCOUNTS ************************
          IF SY-SUBRC = 0.



****** COMMON VARIABLES SET
            CLEAR: LS_DRIVERS, OFFSET.
            SOURCE_PERIOD = <LS_TIME>.
            SOURCE_ACCOUNT = <LS_ACCOUNT>.
            SOURCE_ANALYSIS = <LS_ANALYSIS>.
            SOURCE_SIGNEDDATA = <LS_SIGNEDDATA>.

****** NO TIME CHANGE ********************************
***** F_UNBILLED
            <LS_ANALYSIS> = 'F_UNBILLED'.
            <LS_ACCOUNT> = 'AC210030'.
            <LS_SIGNEDDATA> = SOURCE_SIGNEDDATA * -1.
            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

****** TIME OFFSET BILLING ***************************
            " FIRST FIND THE BILLING OFFSET TIME (IF ANY) -  IF Billing Offset not found, carry on within the current period.
            CLEAR LS_DRIVERS.
            READ TABLE LT_DRIVERS INTO LS_DRIVERS WITH KEY
            ACCOUNT = SOURCE_ACCOUNT
            ANALYSIS = SOURCE_ANALYSIS
            CATEGORY = <LS_CATEGORY>
            ENTITY = <LS_ENTITY>
            AUDITTRAIL = 'DR_BILLOFFSET'.

            OFFSET = LS_DRIVERS-SIGNEDDATA.
            "Found or not?
            IF LS_DRIVERS IS NOT INITIAL.
              "If found - take offset value and get target period by passing source period

              TARGET_PERIOD = LO_TOOLS->PERIOD_OFFSET(
                EXPORTING
                  IMPORT_PERIOD = SOURCE_PERIOD
                  OFFSET        = OFFSET ).

            ELSE.
              "If not found - take source period and write to target
              TARGET_PERIOD = SOURCE_PERIOD.
            ENDIF.
            "Got billing offset

            "1. With billing offset - F_TRBILLDEBT

            <LS_ANALYSIS> = 'F_TRFBILLDEBT'.
            <LS_ACCOUNT> = 'AC210030'.
            <LS_TIME> = TARGET_PERIOD.
            <LS_SIGNEDDATA> = SOURCE_SIGNEDDATA.
            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

            "2. With billing offset - F_BILLED

            <LS_ANALYSIS> = 'F_BILLED'.
            <LS_ACCOUNT> = 'AC210000'.
            <LS_TIME> = TARGET_PERIOD.
            <LS_SIGNEDDATA> = SOURCE_SIGNEDDATA * -1.
            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

            "3. With Billing Offset - IndirectTax
            CLEAR LS_DRIVERS.
            READ TABLE LT_DRIVERS INTO LS_DRIVERS WITH KEY
            ACCOUNT = SOURCE_ACCOUNT
            ANALYSIS = SOURCE_ANALYSIS
            CATEGORY = <LS_CATEGORY>
            ENTITY = <LS_ENTITY>
            AUDITTRAIL = 'DR_INDIRECTTAX'.

            IF LS_DRIVERS IS INITIAL.
              TAX_DRIVER = 0.
            ELSE.
              TAX_DRIVER = LS_DRIVERS-SIGNEDDATA.
            ENDIF.

            "3.1 Write Indirect Tax into F_SALESTAXCR
            <LS_ANALYSIS> = 'F_SALESTAXCR'.
            <LS_ACCOUNT> = 'AC210000'.
            <LS_TIME> = TARGET_PERIOD.
            <LS_SIGNEDDATA> = SOURCE_SIGNEDDATA * TAX_DRIVER * -1.
            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            "3.2 Write Inderict Tax account AC310000
            <LS_ANALYSIS> = 'F_SALESTAXCR'.
            <LS_ACCOUNT> = 'AC310000'.
            <LS_TIME> = TARGET_PERIOD.
            <LS_SIGNEDDATA> = SOURCE_SIGNEDDATA * TAX_DRIVER.
            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
****** BILLING OFFSET IS DONE ***************************************

****** TIME OFFSET CASH  ********************************************
            " FIRST FIND THE CASH OFFSET TIME (IF ANY) -  IF Cash Offset not found, carry on within the current period.
            CLEAR LS_DRIVERS.
            READ TABLE LT_DRIVERS INTO LS_DRIVERS WITH KEY
            ACCOUNT = SOURCE_ACCOUNT
            ANALYSIS = SOURCE_ANALYSIS
            CATEGORY = <LS_CATEGORY>
            ENTITY = <LS_ENTITY>
            AUDITTRAIL = 'DR_CASHOFFSET'.

            OFFSET = OFFSET + LS_DRIVERS-SIGNEDDATA.

            "Found or not?
*            IF LS_DRIVERS IS NOT INITIAL.
            "If found - take offset value and get target period by passing source period

            TARGET_PERIOD = LO_TOOLS->PERIOD_OFFSET(
                EXPORTING
                  IMPORT_PERIOD = SOURCE_PERIOD
                  OFFSET        = OFFSET ).
*            ELSE.
*              "If not found - take source period and write to target
*              TARGET_PERIOD = SOURCE_PERIOD.
*            ENDIF.
            "Got cash offset

            "1. Write F_CASH_CALC Ind Tax

            <LS_ANALYSIS> = 'F_CASH_CALC'.
            <LS_ACCOUNT> = 'AC210000'.
            <LS_TIME> = TARGET_PERIOD.
            "IF TAX_DRIVER = 0.
            "  <LS_SIGNEDDATA> = SOURCE_SIGNEDDATA * TAX_DRIVER.
            "ELSE.
            <LS_SIGNEDDATA> = SOURCE_SIGNEDDATA * ( 1 + TAX_DRIVER ).
            "ENDIF.
            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

****** TIME OFFSET CASH IS DONE *************************************

******* ACR CALCULATIONS END ****************************************

          ELSE.
            " CONTINUE.
          ENDIF.
        ENDLOOP.

        "APPEND LINES OF <LT_FINAL> TO CT_DATA.
        CT_DATA = <LT_FINAL>. "Change per defect #2230





      WHEN 'SUB5'. "Purchasing Tax

        DATA: ENTITY_BS TYPE UJ_DIM_MEMBER.

***** GET ENTITY FROM IT_CV **********************************

        READ TABLE IT_CV INTO CV_ENTITY WITH TABLE KEY DIM_UPPER_CASE = 'ENTITY'.

        CLEAR: LT_SEL, LS_SEL.
        COUNTER = 0.

        LOOP AT CV_ENTITY-MEMBER INTO WA_MEMBER.


          LS_SEL-DIMENSION = 'ENTITY'.
          LS_SEL-ATTRIBUTE = 'ID'.
          LS_SEL-SIGN = 'I'.
          LS_SEL-OPTION = 'EQ'.
          LS_SEL-LOW = WA_MEMBER.
          "Are we having tax change over here?

          IF WA_MEMBER+12(2) = 'BS'.
            ENTITY_BS = WA_MEMBER(12).

            ENTITY = LO_TOOLS->GET_PARENT_MD(
                        EXPORTING
                I_DIMENSION_ID  = LS_SEL-DIMENSION
                PARENT_MBR = ENTITY_BS ).

            LOOP AT ENTITY INTO LS_SEL-LOW.
              APPEND LS_SEL TO LT_SEL.
            ENDLOOP.

            CLEAR ENTITY_BS.

**** Expand time to scope when Tax Change
            IF COUNTER = 0.
              LOOP AT LT_TIMESCOPE INTO LS_TIME.

                LS_SEL-DIMENSION = 'TIME'.
                LS_SEL-ATTRIBUTE = 'ID'.
                LS_SEL-SIGN = 'I'.
                LS_SEL-OPTION = 'EQ'.
                LS_SEL-LOW = LS_TIME-ID.
                APPEND LS_SEL TO LT_SEL.

              ENDLOOP.
              COUNTER = 1.
            ENDIF.

            LS_SEL-DIMENSION = 'ENTITY'.
            LS_SEL-ATTRIBUTE = 'ID'.
            LS_SEL-SIGN = 'I'.
            LS_SEL-OPTION = 'EQ'.



          ELSE.

            APPEND LS_SEL TO LT_SEL.

          ENDIF.



        ENDLOOP.

*******************************************************************

******** GET MASTER DATA FOR ENTITY ******************************
        LO_TOOLS->READ_MASTER_DATA(
          EXPORTING
            I_DIMENSION_ID   = LS_SEL-DIMENSION
          IMPORTING
            OUTPUT_R_DATA = MASTER_DATA
          CHANGING
            LT_SEL        = LT_SEL ).

        ASSIGN MASTER_DATA->* TO <DIMENSION_DATA>.
        MOVE-CORRESPONDING <DIMENSION_DATA> TO LT_ENTITY.
*******************************************************************
*********** SET DRIVER SEL ****************************************

        LOOP AT LT_ENTITY INTO LS_ENTITY.

          CONCATENATE LS_ENTITY-PROFIT_CENTRE 'BS' INTO LS_SEL-LOW.
          APPEND LS_SEL TO LT_DRIVER_SEL. "FOR DRIVERS SELECTION

        ENDLOOP.


*******************************************************************


        LS_SEL-DIMENSION = 'ACCOUNT'.
        LS_SEL-ATTRIBUTE = 'ID'.
        LS_SEL-SIGN = 'I'.
        LS_SEL-OPTION = 'EQ'.
        LS_SEL-LOW = 'AC300000'.
        APPEND LS_SEL TO LT_SEL.

**** add additional accounts for zeroing out *************************
        LS_SEL-LOW = 'DR001'.
        APPEND LS_SEL TO LT_DRIVER_SEL. "FOR DRIVERS SELECTION
        LS_SEL-LOW = 'AC310000'.
        APPEND LS_SEL TO LT_SEL.
**********************************************************************


        CLEAR: LS_SEL, WA_MEMBER.

*        LOOP AT CV_CATEGORY-MEMBER INTO WA_MEMBER.
*        Change 5 implemented
          LS_SEL-DIMENSION = 'CATEGORY'.
          LS_SEL-ATTRIBUTE = 'ID'.
          LS_SEL-SIGN = 'I'.
          LS_SEL-OPTION = 'EQ'.
          LS_SEL-LOW = WORKING_CATEGORY.
*          LS_SEL-LOW = WA_MEMBER.
          APPEND LS_SEL TO LT_SEL.
          APPEND LS_SEL TO LT_DRIVER_SEL. "FOR DRIVERS SELECTION

*        ENDLOOP.


        READ TABLE IT_CV INTO CV_AUDITTRAIL WITH TABLE KEY DIM_UPPER_CASE = 'AUDITTRAIL'.

        CLEAR: LS_SEL, WA_MEMBER, COUNTER.

        LOOP AT CV_AUDITTRAIL-MEMBER INTO WA_MEMBER.

          LS_SEL-DIMENSION = 'AUDITTRAIL'.
          LS_SEL-ATTRIBUTE = 'ID'.
          LS_SEL-SIGN = 'I'.
          LS_SEL-OPTION = 'EQ'.
          LS_SEL-LOW = WA_MEMBER.
          "***********defect CMC002 change start *****************
          IF WA_MEMBER = 'CAPEX_INPUT'
            OR WA_MEMBER = 'RESOURCE_INPUT'". Change 6 Remove
            OR WA_MEMBER = 'RESOURCE_ITSCREDIT'. "Change 6 Add
            WA_MEMBER = 'EXIT'.
            EXIT.
          ELSE.
            APPEND LS_SEL TO LT_SEL.
          ENDIF.
          "***********defect CMC002 change end *******************

        ENDLOOP.

        IF WA_MEMBER = 'EXIT'.
          EXIT.
        ENDIF.

        LS_SEL-LOW = 'INPUT'.
        APPEND LS_SEL TO LT_DRIVER_SEL.


        LS_SEL-DIMENSION = 'PARTNER'.
        LS_SEL-ATTRIBUTE = 'ID'.
        LS_SEL-SIGN = 'I'.
        LS_SEL-OPTION = 'EQ'.
        LS_SEL-LOW = 'I_NONE'.
        APPEND LS_SEL TO LT_SEL.


**** add additional analysis to zero out *******************************

        LS_SEL-DIMENSION = 'ANALYSIS'.
        LS_SEL-ATTRIBUTE = 'ID'.
        LS_SEL-SIGN = 'I'.
        LS_SEL-OPTION = 'EQ'.
        LS_SEL-LOW = 'NO_ANALYSIS'.
        APPEND LS_SEL TO LT_DRIVER_SEL. "FOR DRIVERS SELECTION
        LS_SEL-LOW = 'F_PURCHASETAX'.
        APPEND LS_SEL TO LT_SEL.
        LS_SEL-LOW = 'F_ISMVT'.
        APPEND LS_SEL TO LT_SEL.
************************************************************************


        CLEAR: LS_SEL, WA_MEMBER, LS_TIME.

        READ TABLE IT_CV INTO CV_TIME WITH TABLE KEY DIM_UPPER_CASE = 'TIME'.

        LOOP AT CV_TIME-MEMBER INTO WA_MEMBER.

          CHECK WA_MEMBER <> 'XXXX.INP'.

          LS_SEL-DIMENSION = 'TIME'.
          LS_SEL-ATTRIBUTE = 'ID'.
          LS_SEL-SIGN = 'I'.
          LS_SEL-OPTION = 'EQ'.
          LS_SEL-LOW = WA_MEMBER.
          APPEND LS_SEL TO LT_SEL.

        ENDLOOP.

        LS_SEL-LOW = 'XXXX.INP'.
        APPEND LS_SEL TO LT_DRIVER_SEL.

        SORT LT_SEL BY LOW.
        DELETE ADJACENT DUPLICATES FROM LT_SEL.

        LO_TOOLS->READ_MODEL_DATA(
          EXPORTING
            IT_SEL      = LT_SEL
          IMPORTING
            OUTPUT_DATA = <CT_DATA> ).

        CLEAR CT_DATA.
        MOVE-CORRESPONDING <CT_DATA> TO CT_DATA.

*** WRITE ZEROES *******

        CLEAR: LT_SEL, LS_SEL.


        LOOP AT CT_DATA ASSIGNING <LS_RESULT_REC>.
          ASSIGN COMPONENT 'ACCOUNT' OF STRUCTURE <LS_RESULT_REC> TO <LS_ACCOUNT>.
          ASSIGN COMPONENT 'ANALYSIS' OF STRUCTURE <LS_RESULT_REC> TO <LS_ANALYSIS>.
          ASSIGN COMPONENT 'PARTNER' OF STRUCTURE <LS_RESULT_REC> TO <LS_PARTNER>.
          ASSIGN COMPONENT 'SIGNEDDATA' OF STRUCTURE <LS_RESULT_REC> TO <LS_SIGNEDDATA>.

          IF ( <LS_ACCOUNT> = 'AC310000' OR <LS_ACCOUNT> = 'AC300000')
            AND <LS_ANALYSIS> = 'F_PURCHASETAX'.
            <LS_SIGNEDDATA> = 0.
            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
          ENDIF.

        ENDLOOP.


***************************************  READ RELEVANT DRIVERS ***********************************

        CLEAR: LS_SEL, <CT_DATA>.

        LS_SEL-DIMENSION = 'PARTNER'.
        LS_SEL-ATTRIBUTE = 'ID'.
        LS_SEL-SIGN = 'I'.
        LS_SEL-OPTION = 'EQ'.
        LS_SEL-LOW = 'I_NONE'.
        APPEND LS_SEL TO LT_DRIVER_SEL. "FOR DRIVERS SELECTION

        SORT LT_DRIVER_SEL BY LOW.
        DELETE ADJACENT DUPLICATES FROM LT_DRIVER_SEL.

        LO_TOOLS->READ_MODEL_DATA(
          EXPORTING
            IT_SEL      = LT_DRIVER_SEL
          IMPORTING
            OUTPUT_DATA = <CT_DATA> ).

        MOVE-CORRESPONDING <CT_DATA> TO LT_DRIVERS. "<----- Reference LT_DRIVERS table for all your driving needs!

***************************************************************************************************


        CREATE DATA LS_RESULT_REC LIKE LINE OF CT_DATA.
        ASSIGN LS_RESULT_REC->* TO <LS_RESULT_REC>.

**** LOOP **************

        CLEAR: LS_ENTITY, ENTITY_BS.

        LOOP AT CT_DATA ASSIGNING <LS_REC>.
          <LS_RESULT_REC> = <LS_REC>.
          ASSIGN COMPONENT 'ACCOUNT' OF STRUCTURE <LS_RESULT_REC> TO <LS_ACCOUNT>.
          ASSIGN COMPONENT 'ANALYSIS' OF STRUCTURE <LS_RESULT_REC> TO <LS_ANALYSIS>.
          ASSIGN COMPONENT 'ENTITY' OF STRUCTURE <LS_RESULT_REC> TO <LS_ENTITY>.
          ASSIGN COMPONENT 'PARTNER' OF STRUCTURE <LS_RESULT_REC> TO <LS_PARTNER>.
          ASSIGN COMPONENT 'TIME' OF STRUCTURE <LS_RESULT_REC> TO <LS_TIME>.
          ASSIGN COMPONENT 'CATEGORY' OF STRUCTURE <LS_RESULT_REC> TO <LS_CATEGORY>.
          ASSIGN COMPONENT 'SIGNEDDATA' OF STRUCTURE <LS_RESULT_REC> TO <LS_SIGNEDDATA>.

          CLEAR LS_DRIVERS.

          IF <LS_ACCOUNT> = 'AC300000' AND <LS_ANALYSIS> = 'F_ISMVT'.

            READ TABLE LT_ENTITY WITH KEY ID = <LS_ENTITY> INTO LS_ENTITY.
            CONCATENATE LS_ENTITY-PROFIT_CENTRE 'BS' INTO ENTITY_BS.

            READ TABLE LT_DRIVERS INTO LS_DRIVERS WITH KEY
            ACCOUNT = 'DR001'
            ANALYSIS = 'NO_ANALYSIS'
            CATEGORY = <LS_CATEGORY>
            ENTITY = ENTITY_BS
            PARTNER = 'I_NONE'
            AUDITTRAIL = 'INPUT'.

            IF LS_DRIVERS IS INITIAL.
              TAX_DRIVER = 0.
            ELSE.
              TAX_DRIVER = LS_DRIVERS-SIGNEDDATA.
            ENDIF.

            <LS_ACCOUNT> = 'AC300000'.
            <LS_ANALYSIS> = 'F_PURCHASETAX'.
            <LS_SIGNEDDATA> = <LS_SIGNEDDATA> * TAX_DRIVER.
            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

            <LS_ACCOUNT> = 'AC310000'.
            <LS_ANALYSIS> = 'F_PURCHASETAX'.
            <LS_SIGNEDDATA> = <LS_SIGNEDDATA> * -1.
            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.


          ENDIF.


        ENDLOOP.

        "APPEND LINES OF <LT_FINAL> TO CT_DATA.
        CT_DATA = <LT_FINAL>. "Change per defect #2230



*****************************************************************************************************
*****************************************************************************************************
*****************************************************************************************************
*****************************************************************************************************

      WHEN 'SUB6'. "IC Matching Operations


***** GET ACCOUNTS WHERE DIMLIST = ICC or ICS
        CLEAR: LT_SEL, LS_SEL.

        LS_SEL-DIMENSION = 'ACCOUNT'.
        LS_SEL-ATTRIBUTE = 'DIMLIST'.
        LS_SEL-SIGN = 'I'.
        LS_SEL-OPTION = 'CP'.
        LS_SEL-LOW = 'IC*'.

        APPEND LS_SEL TO LT_SEL.


        LO_TOOLS->READ_MASTER_DATA(
          EXPORTING
            I_DIMENSION_ID   = 'ACCOUNT'
          IMPORTING
            OUTPUT_R_DATA = MASTER_DATA
          CHANGING
            LT_SEL        = LT_SEL ).

        ASSIGN MASTER_DATA->* TO <DIMENSION_DATA>.
        MOVE-CORRESPONDING <DIMENSION_DATA> TO LT_ACCOUNT.

*** PREPARE READ FROM MODEL**********************************

***** GET ENTITY FROM IT_CV **********************************

        READ TABLE IT_CV INTO CV_ENTITY WITH TABLE KEY DIM_UPPER_CASE = 'ENTITY'.


        CLEAR: LT_SEL, LS_SEL.

        LOOP AT CV_ENTITY-MEMBER INTO WA_MEMBER.


          LS_SEL-DIMENSION = 'ENTITY'.
          LS_SEL-ATTRIBUTE = 'ID'.
          LS_SEL-SIGN = 'I'.
          LS_SEL-OPTION = 'EQ'.
          LS_SEL-LOW = WA_MEMBER.
          APPEND LS_SEL TO LT_SEL.
          APPEND LS_SEL TO LT_DRIVER_SEL. "FOR DRIVERS SELECTION
        ENDLOOP.

******** GET MASTER DATA FOR ENTITY ******************************
        LO_TOOLS->READ_MASTER_DATA(
          EXPORTING
            I_DIMENSION_ID   = LS_SEL-DIMENSION
          IMPORTING
            OUTPUT_R_DATA = MASTER_DATA
          CHANGING
            LT_SEL        = LT_SEL ).

        ASSIGN MASTER_DATA->* TO <DIMENSION_DATA>.
        MOVE-CORRESPONDING <DIMENSION_DATA> TO LT_ENTITY.
*******************************************************************




        CLEAR: LS_SEL.

        LOOP AT LT_ACCOUNT INTO LS_ACCOUNT.

          LS_SEL-DIMENSION = 'ACCOUNT'.
          LS_SEL-ATTRIBUTE = 'ID'.
          LS_SEL-SIGN = 'I'.
          LS_SEL-OPTION = 'EQ'.
          LS_SEL-LOW = LS_ACCOUNT-ID.
          APPEND LS_SEL TO LT_SEL.
          APPEND LS_SEL TO LT_DRIVER_SEL. "FOR DRIVERS SELECTION
        ENDLOOP.

**** add additional accounts for zeroing out *************************
        LS_SEL-LOW = 'AC310000'.
        APPEND LS_SEL TO LT_SEL.
        LS_SEL-LOW = 'AC213000'.
        APPEND LS_SEL TO LT_SEL.
        LS_SEL-LOW = 'AC213010'.
        APPEND LS_SEL TO LT_SEL.
        LS_SEL-LOW = 'AC220000'.
        APPEND LS_SEL TO LT_SEL.
**********************************************************************


        CLEAR: LS_SEL, WA_MEMBER.

*        LOOP AT CV_CATEGORY-MEMBER INTO WA_MEMBER.

          LS_SEL-DIMENSION = 'CATEGORY'.
          LS_SEL-ATTRIBUTE = 'ID'.
          LS_SEL-SIGN = 'I'.
          LS_SEL-OPTION = 'EQ'.
          LS_SEL-LOW = WORKING_CATEGORY.
*          LS_SEL-LOW = WA_MEMBER.
          APPEND LS_SEL TO LT_SEL.
          APPEND LS_SEL TO LT_DRIVER_SEL. "FOR DRIVERS SELECTION

*        ENDLOOP.

        READ TABLE IT_CV INTO CV_AUDITTRAIL WITH TABLE KEY DIM_UPPER_CASE = 'AUDITTRAIL'.

        CLEAR: LS_SEL, WA_MEMBER, COUNTER.

        LOOP AT CV_AUDITTRAIL-MEMBER INTO WA_MEMBER.

          LS_SEL-DIMENSION = 'AUDITTRAIL'.
          LS_SEL-ATTRIBUTE = 'ID'.
          LS_SEL-SIGN = 'I'.
          LS_SEL-OPTION = 'EQ'.
          LS_SEL-LOW = WA_MEMBER.

          "***********defect CMC002 change start *****************
          IF WA_MEMBER = 'CAPEX_INPUT'
            OR WA_MEMBER = 'RESOURCE_INPUT'". Change 6 Remove
            OR WA_MEMBER = 'RESOURCE_ITSCREDIT'. "Change 6 Add
            WA_MEMBER = 'EXIT'.
            EXIT.
          ELSE.
            APPEND LS_SEL TO LT_SEL.
          ENDIF.
          "***********defect CMC002 change end *******************

**** add additional audittrail to read data if drivers are being changed

          IF ( WA_MEMBER = 'DR_INDIRECTTAX' ) AND COUNTER = 0.
            DELETE LT_SEL WHERE LOW =  WA_MEMBER.
            LS_SEL-LOW = 'INPUT'.
            APPEND LS_SEL TO LT_SEL.

**** Expand time to scope while INDIRECTTAX
            LOOP AT LT_TIMESCOPE INTO LS_TIME.

              LS_SEL-DIMENSION = 'TIME'.
              LS_SEL-ATTRIBUTE = 'ID'.
              LS_SEL-SIGN = 'I'.
              LS_SEL-OPTION = 'EQ'.
              LS_SEL-LOW = LS_TIME-ID.
              APPEND LS_SEL TO LT_SEL.

            ENDLOOP.


******************************************

            COUNTER = 1.
          ENDIF.
*************************************************************************

        ENDLOOP.

        IF WA_MEMBER = 'EXIT'.
          EXIT.
        ENDIF.

        CLEAR: LS_SEL, WA_MEMBER.

        LOOP AT ANALYSIS INTO WA_MEMBER.

          LS_SEL-DIMENSION = 'ANALYSIS'.
          LS_SEL-ATTRIBUTE = 'ID'.
          LS_SEL-SIGN = 'I'.
          LS_SEL-OPTION = 'EQ'.
          LS_SEL-LOW = WA_MEMBER.
          APPEND LS_SEL TO LT_SEL.
          APPEND LS_SEL TO LT_DRIVER_SEL. "FOR DRIVERS SELECTION

        ENDLOOP.
**** add additional analysis to zero out *******************************
        LS_SEL-LOW = 'F_ISMVT'.
        APPEND LS_SEL TO LT_SEL.
        LS_SEL-LOW = 'F_PURCHASETAX'.
        APPEND LS_SEL TO LT_SEL.
        LS_SEL-LOW = 'F_SALESTAXCR'.
************************************************************************
        CLEAR: LS_SEL.

        LS_SEL-DIMENSION = 'PARTNER'.
        LS_SEL-ATTRIBUTE = 'ID'.
        LS_SEL-SIGN = 'I'.
        LS_SEL-OPTION = 'NE'.
        LS_SEL-LOW = 'I_NONE'.
        APPEND LS_SEL TO LT_SEL.


        CLEAR: LS_SEL, WA_MEMBER, LS_TIME.

        READ TABLE IT_CV INTO CV_TIME WITH TABLE KEY DIM_UPPER_CASE = 'TIME'.

        LOOP AT CV_TIME-MEMBER INTO WA_MEMBER.

          CHECK WA_MEMBER <> 'XXXX.INP'.

          LS_SEL-DIMENSION = 'TIME'.
          LS_SEL-ATTRIBUTE = 'ID'.
          LS_SEL-SIGN = 'I'.
          LS_SEL-OPTION = 'EQ'.
          LS_SEL-LOW = WA_MEMBER.
          APPEND LS_SEL TO LT_SEL.

        ENDLOOP.


        SORT LT_SEL BY LOW.
        DELETE ADJACENT DUPLICATES FROM LT_SEL.

        LO_TOOLS->READ_MODEL_DATA(
          EXPORTING
            IT_SEL      = LT_SEL
          IMPORTING
            OUTPUT_DATA = <CT_DATA> ).

        CLEAR CT_DATA.
        MOVE-CORRESPONDING <CT_DATA> TO CT_DATA.

***************************************  READ RELEVANT DRIVERS ***********************************

        CLEAR: LS_SEL, <CT_DATA>.

        LS_SEL-DIMENSION = 'TIME'.
        LS_SEL-ATTRIBUTE = 'ID'.
        LS_SEL-SIGN = 'I'.
        LS_SEL-OPTION = 'EQ'.
        LS_SEL-LOW = 'XXXX.INP'.
        APPEND LS_SEL TO LT_DRIVER_SEL.

        LS_SEL-DIMENSION = 'AUDITTRAIL'.
        LS_SEL-ATTRIBUTE = 'ID'.
        LS_SEL-SIGN = 'I'.
        LS_SEL-OPTION = 'EQ'.
        LS_SEL-LOW = 'DR_INDIRECTTAX'.
        APPEND LS_SEL TO LT_DRIVER_SEL.

        LO_TOOLS->READ_MODEL_DATA(
          EXPORTING
            IT_SEL      = LT_DRIVER_SEL
          IMPORTING
            OUTPUT_DATA = <CT_DATA> ).

        MOVE-CORRESPONDING <CT_DATA> TO LT_DRIVERS. "<----- Reference LT_DRIVERS table for all your driving needs!

***************************************************************************************************
*** WRITE ZEROES *******

        CLEAR: LT_SEL, LS_SEL.

**** SET PROPER PARTNER ************
        LS_SEL-DIMENSION = 'PARTNER'.
        LS_SEL-ATTRIBUTE = 'ID'.
        LS_SEL-SIGN = 'I'.
        LS_SEL-OPTION = 'EQ'.
************************************

        LOOP AT CT_DATA ASSIGNING <LS_RESULT_REC>.
          ASSIGN COMPONENT 'ACCOUNT' OF STRUCTURE <LS_RESULT_REC> TO <LS_ACCOUNT>.
          ASSIGN COMPONENT 'ANALYSIS' OF STRUCTURE <LS_RESULT_REC> TO <LS_ANALYSIS>.
          ASSIGN COMPONENT 'PARTNER' OF STRUCTURE <LS_RESULT_REC> TO <LS_PARTNER>.
          ASSIGN COMPONENT 'SIGNEDDATA' OF STRUCTURE <LS_RESULT_REC> TO <LS_SIGNEDDATA>.
**** SET PROPER PARTNER ************
          LS_SEL-LOW = <LS_PARTNER>.
          APPEND LS_SEL TO LT_SEL.
************************************

          IF ( <LS_ACCOUNT> = 'AC213000' OR <LS_ACCOUNT> = 'AC220000' OR <LS_ACCOUNT> = 'AC213010' )
            AND <LS_ANALYSIS> = 'F_ISMVT'.
            <LS_SIGNEDDATA> = 0.
            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

          ELSEIF ( <LS_ACCOUNT> = 'AC213010' OR <LS_ACCOUNT> = 'AC310000' )
            AND <LS_ANALYSIS> = 'F_PURCHASETAX'.
            <LS_SIGNEDDATA> = 0.
            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

          ENDIF.
          IF ( <LS_ACCOUNT> = 'AC310000' OR <LS_ACCOUNT> = 'AC213000' )
            AND <LS_ANALYSIS> = 'F_SALESTAXCR'.
            <LS_SIGNEDDATA> = 0.
            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

          ENDIF.
        ENDLOOP.

        SORT LT_SEL.
        DELETE ADJACENT DUPLICATES FROM LT_SEL.
******** GET MASTER DATA FOR PARTNER******************************
        LO_TOOLS->READ_MASTER_DATA(
          EXPORTING
            I_DIMENSION_ID   = LS_SEL-DIMENSION
          IMPORTING
            OUTPUT_R_DATA = MASTER_DATA
          CHANGING
            LT_SEL        = LT_SEL ).

        ASSIGN MASTER_DATA->* TO <DIMENSION_DATA>.
        MOVE-CORRESPONDING <DIMENSION_DATA> TO LT_PARTNER.
******************************************************************

        CREATE DATA LS_RESULT_REC LIKE LINE OF CT_DATA.
        ASSIGN LS_RESULT_REC->* TO <LS_RESULT_REC>.

**** LOOP **************


        LOOP AT CT_DATA ASSIGNING <LS_REC>.
          <LS_RESULT_REC> = <LS_REC>.
          ASSIGN COMPONENT 'ACCOUNT' OF STRUCTURE <LS_RESULT_REC> TO <LS_ACCOUNT>.
          ASSIGN COMPONENT 'ANALYSIS' OF STRUCTURE <LS_RESULT_REC> TO <LS_ANALYSIS>.
          ASSIGN COMPONENT 'ENTITY' OF STRUCTURE <LS_RESULT_REC> TO <LS_ENTITY>.
          ASSIGN COMPONENT 'PARTNER' OF STRUCTURE <LS_RESULT_REC> TO <LS_PARTNER>.
          ASSIGN COMPONENT 'TIME' OF STRUCTURE <LS_RESULT_REC> TO <LS_TIME>.
          ASSIGN COMPONENT 'CATEGORY' OF STRUCTURE <LS_RESULT_REC> TO <LS_CATEGORY>.
          ASSIGN COMPONENT 'SIGNEDDATA' OF STRUCTURE <LS_RESULT_REC> TO <LS_SIGNEDDATA>.

          CLEAR: LS_ENTITY, LS_PARTNER, LS_ACCOUNT, LS_DRIVERS.

          READ TABLE LT_ENTITY WITH KEY ID = <LS_ENTITY> INTO LS_ENTITY.
          READ TABLE LT_PARTNER WITH KEY ID = <LS_PARTNER> INTO LS_PARTNER.
          READ TABLE LT_ACCOUNT WITH KEY ID = <LS_ACCOUNT> INTO LS_ACCOUNT.
**** MAIN CLAUSES ***********************************************************************

          IF LS_ACCOUNT IS NOT INITIAL.

            SOURCE_ACCOUNT = <LS_ACCOUNT>.
            SOURCE_ANALYSIS = <LS_ANALYSIS>.
            SOURCE_SIGNEDDATA = <LS_SIGNEDDATA>.

            "CR GROUP RECHARGES START
            ASSIGN COMPONENT 'AUDITTRAIL' OF STRUCTURE <LS_RESULT_REC> TO <LS_AUDITTRAIL>.

            IF <LS_AUDITTRAIL> = 'BW_MW'.
              VAR_AUDITTRAIL = 'BW_MW_TAX'.
            ELSE.
              VAR_AUDITTRAIL = 'DR_INDIRECTTAX'.
            ENDIF.
            "CR GROUP RECHARGES END



            READ TABLE LT_DRIVERS INTO LS_DRIVERS WITH KEY
            ACCOUNT = SOURCE_ACCOUNT
            ANALYSIS = SOURCE_ANALYSIS
            CATEGORY = <LS_CATEGORY>
            ENTITY = <LS_ENTITY>
            PARTNER = <LS_PARTNER>

            "CR GROUP RECHARGES START
            AUDITTRAIL = VAR_AUDITTRAIL.
            "AUDITTRAIL = 'DR_INDIRECTTAX'.
            "CR GROUP RECHARGES END

            IF LS_DRIVERS IS INITIAL.
              TAX_DRIVER = 0.
            ELSE.
              TAX_DRIVER = LS_DRIVERS-SIGNEDDATA.
            ENDIF.



            IF LS_ENTITY-COMPANY_CODE <> LS_PARTNER-COMPANY_CODE. "Intercompany

              "CR GROUP RECHARGES START
              IF LS_ACCOUNT-DIMLIST = 'ICS' OR ( LS_ACCOUNT-DIMLIST = 'ICRCG' AND SOURCE_SIGNEDDATA < 0 ).
                "IF LS_ACCOUNT-DIMLIST = 'ICS'.

                IF <LS_AUDITTRAIL> = 'BW_MW' . "WO0000002040082 IF VAR_AUDITTRAIL = 'BW_MW' .
                  <LS_ACCOUNT> = 'AC220000'.
                ELSE.
                  <LS_ACCOUNT> = 'AC213000'.
                ENDIF.

                "<LS_ACCOUNT> = 'AC213000'.

                "CR GROUP RECHARGES END
                <LS_SIGNEDDATA> = SOURCE_SIGNEDDATA * -1.
                <LS_ANALYSIS> = 'F_ISMVT'.
                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.


                "CR GROUP RECHARGES START
                IF <LS_AUDITTRAIL> = 'BW_MW' . "WO0000002040082 IF VAR_AUDITTRAIL = 'BW_MW' .
                  <LS_ACCOUNT> = 'AC220000'.
                ELSE.
                  <LS_ACCOUNT> = 'AC213000'.
                ENDIF.

                "<LS_ACCOUNT> = 'AC213000'.

                "CR GROUP RECHARGES END

                <LS_SIGNEDDATA> = SOURCE_SIGNEDDATA * TAX_DRIVER * -1.
                <LS_ANALYSIS> = 'F_SALESTAXCR'.
                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

                <LS_ACCOUNT> = 'AC310000'.
                <LS_SIGNEDDATA> = SOURCE_SIGNEDDATA * TAX_DRIVER. " * -1. Change #1
                <LS_ANALYSIS> = 'F_SALESTAXCR'.
                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

                "CR GROUP RECHARGES START
              ELSEIF LS_ACCOUNT-DIMLIST = 'ICC'  OR ( LS_ACCOUNT-DIMLIST = 'ICRCG' AND SOURCE_SIGNEDDATA > 0 ).

                "ELSEIF LS_ACCOUNT-DIMLIST = 'ICC'.
                "CR GROUP RECHARGES END


                "CR GROUP RECHARGES START
                IF <LS_AUDITTRAIL> = 'BW_MW' . "WO0000002040082 IF VAR_AUDITTRAIL = 'BW_MW' .
                  <LS_ACCOUNT> = 'AC220000'.
                ELSE.
                  <LS_ACCOUNT> = 'AC213010'.
                ENDIF.

                "<LS_ACCOUNT> = 'AC213010'.

                "CR GROUP RECHARGES END

                <LS_SIGNEDDATA> = SOURCE_SIGNEDDATA * -1.
                <LS_ANALYSIS> = 'F_ISMVT'.
                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

                "CR GROUP RECHARGES START
                IF <LS_AUDITTRAIL> = 'BW_MW'  . "WO0000002040082 IF VAR_AUDITTRAIL = 'BW_MW' .
                  <LS_ACCOUNT> = 'AC220000'.
                ELSE.
                  <LS_ACCOUNT> = 'AC213010'.
                ENDIF.

                "<LS_ACCOUNT> = 'AC213010'.

                "CR GROUP RECHARGES END
                <LS_SIGNEDDATA> = SOURCE_SIGNEDDATA * TAX_DRIVER * -1.
                <LS_ANALYSIS> = 'F_PURCHASETAX'.
                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

                <LS_ACCOUNT> = 'AC310000'.
                <LS_SIGNEDDATA> = SOURCE_SIGNEDDATA * TAX_DRIVER. " * -1. Change #1
                <LS_ANALYSIS> = 'F_PURCHASETAX'.
                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

              ENDIF.

            ELSE. "Intracompany

              "CR GROUP RECHARGES START
              IF LS_ACCOUNT-DIMLIST = 'ICS' OR LS_ACCOUNT-DIMLIST = 'ICC' OR LS_ACCOUNT-DIMLIST = 'ICRCG'.
                "IF LS_ACCOUNT-DIMLIST = 'ICS' OR LS_ACCOUNT-DIMLIST = 'ICC'.
                "CR GROUP RECHARGES END

                <LS_ACCOUNT> = 'AC220000'.
                <LS_SIGNEDDATA> = SOURCE_SIGNEDDATA * -1.
                <LS_ANALYSIS> = 'F_ISMVT'.
                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

              ENDIF.
            ENDIF.

          ENDIF.

**** MAIN CLAUSES END ***********************************************************************
******************************************************************************************

        ENDLOOP.
        "APPEND LINES OF <LT_FINAL> TO CT_DATA.
        CT_DATA = <LT_FINAL>. "Change per defect #2230

**************************************************************
**************************************************************

    ENDCASE.


  ENDMETHOD.


  METHOD IS_ACT_MAIN.
*&---------------------------------------------------------------------*
*&  Method           IS_ACT_MAIN
*&---------------------------------------------------------------------*
****************************************************************************
* Program type....:  Customer SO Custom Method
* Author...........:   ()
* Date.............: February 2017
* ----------------------------------------------------------------------
* Description......: This method does the main actualisation calculations
*                    for INC_STATEMENT model. This includes:
*                    a)Copy ACTUAL data to WFORECAST category in (C - 1) period
*                    b)... - additional
****************************************************************************
* Change Log
****************************************************************************
* Changed on:                 By:
* Description:
****************************************************************************
INCLUDE ZINC_PLAN_BADI_COMMON_VARS. "Common Variables

*** SET TOOLS INSTANCE *****
    CREATE OBJECT LO_TOOLS
      EXPORTING
        I_ENV_ID = I_APPSET_ID
        I_MOD_ID = I_APPL_ID.
***************************************************************
************** ASSIGN WAs *************************************
    CREATE DATA LT_FINAL LIKE CT_DATA.
    ASSIGN LT_FINAL->* TO <LT_FINAL>.
    CREATE DATA LS_RESULT_REC LIKE LINE OF CT_DATA.
    ASSIGN LS_RESULT_REC->* TO <LS_RESULT_REC>.
    CREATE DATA LS_REC LIKE LINE OF CT_DATA.
    ASSIGN LS_REC->* TO <LS_REC>.
************************ASSIGN STRUCTURE OF INCOMING MODEL ****
    LO_TOOLS->WRITE_MODEL_DATA(
      EXPORTING
        TARGET_MODEL = I_APPL_ID "self structure
        WRITE       = 'N'
     IMPORTING
       LR_DATA     = LR_DATA ).

    ASSIGN LR_DATA->* TO <CT_DATA>.
*****************************************************************************

****** GET TIMESCOPE AND VARIABLES
    LO_TOOLS->TIME(
      EXPORTING
        CATEGORY    = WORKING_CATEGORY
      IMPORTING
       CURRENT_PERIOD   = CURRENT_PERIOD ).

" Get Last Period
LO_TOOLS->PERIOD_OFFSET(
  EXPORTING
    IMPORT_PERIOD = CURRENT_PERIOD
    OFFSET        = -1
  RECEIVING
    EXPORT_PERIOD = PREVIOUS_PERIOD ).


******* LS-SEL *********************************
          LS_SEL-ATTRIBUTE = 'ID'.
          LS_SEL-SIGN = 'I'.
          LS_SEL-OPTION = 'EQ'.
************************************************

          LS_SEL-DIMENSION = 'CATEGORY'.
          LS_SEL-LOW = 'ACTUAL'.
          APPEND LS_SEL TO LT_SEL.

          LS_SEL-DIMENSION = 'TIME'.
          LS_SEL-LOW = PREVIOUS_PERIOD.
          APPEND LS_SEL TO LT_SEL.


        LO_TOOLS->READ_MODEL_DATA(
           EXPORTING
             IT_SEL      = LT_SEL
           IMPORTING
             OUTPUT_DATA = <CT_DATA> ).

    MOVE-CORRESPONDING <CT_DATA> TO CT_DATA.


      LOOP AT CT_DATA ASSIGNING <LS_RESULT_REC>.
      ASSIGN COMPONENT 'CATEGORY' OF STRUCTURE <LS_RESULT_REC> TO <LS_CATEGORY>.
      ASSIGN COMPONENT 'TIME' OF STRUCTURE <LS_RESULT_REC> TO <LS_TIME>.
      ASSIGN COMPONENT 'SIGNEDDATA' OF STRUCTURE <LS_RESULT_REC> TO <LS_SIGNEDDATA>.

      <LS_CATEGORY> = WORKING_CATEGORY.

    ENDLOOP.

"APPEND LINES OF <LT_FINAL> TO CT_DATA.

  ENDMETHOD.


  METHOD RESOURCE_CALC.
*&---------------------------------------------------------------------*
****************************************************************************
* Program type....:  Customer SO Custom Method
* Author...........:   ()
* Date.............: January 30,2017
* ----------------------------------------------------------------------
* Description......: This method handles all the relevant calculations
*                    within the RESOURCE model
*
*
****************************************************************************
* Change Log : 1. CR 2586 - Contractors are feeding into subcontractor costs
*                 rather than agency costs.
*
*              2. NEW_002 - SMART calculation - salary sacrifice should be
*                 generating a credit to IS not debit
*
*              3. CHANGE3 - ASPAC ME CONTRACT DRIVER ENHANCEMENT; RECALCS
*                 ENTITIES THAT ARE TIED TO THE PCXXXXXXXXXXBS ENTITY
*
*              4. CHANGE4 - Treat ITS for all the companies the same as for S101
*
*              5. CR3102 - Amount Accrued to BS does not include NI
*
*
*
****************************************************************************
* Changed on: 05.10.2017                By: RDG
* Description:
*
****************************************************************************
*
*
***************** RESOURCE MODEL CALCULATIONS ******************************
*This method is used for RESOURCE model calculation
****************************************************************************
    DATA: BEGIN OF LS_TIME,
            ID        TYPE CHAR32,
            CURRMONTH TYPE CHAR32,
            BUDMONTH  TYPE CHAR32,
            TIMEID    TYPE CHAR32,
            YEAR      TYPE CHAR32,
          END OF LS_TIME.

    DATA: BEGIN OF LS_FX_ROW,
            TIME          TYPE CHAR32,
            CATEGORY      TYPE CHAR32,
            EMPLOYEE      TYPE CHAR32,
            ENTITY        TYPE CHAR32,
            RESOURCE_ACCT TYPE CHAR32,
            CURRENCY      TYPE CHAR32, "CURRENCY FOR OUTPUTLC
            RATETYPE      TYPE CHAR32,
            INPUTGBP      TYPE UJ_SDATA,
            OUTPUTLC      TYPE UJ_SDATA,
          END OF LS_FX_ROW.

    DATA: BEGIN OF LS_CATEGORY,
            ID   TYPE CHAR32,
            YEAR TYPE CHAR32,
          END OF LS_CATEGORY.

    DATA: BEGIN OF LS_RESOURCE,
            AUDITTRAIL    TYPE CHAR32,
            CATEGORY      TYPE CHAR32,
            EMPLOYEE      TYPE CHAR32,
            ENTITY        TYPE CHAR32,
            RESOURCE_ACCT TYPE CHAR32,
            RPTCURRENCY   TYPE CHAR32,
            TIME          TYPE CHAR32,
            SIGNEDDATA    TYPE UJ_SDATA,
          END OF LS_RESOURCE.

    TYPES: BEGIN OF LS_ACCT_DRIVER,
            DRIVER_ACCT   TYPE CHAR32,
            TARGET_ACCT   TYPE CHAR32,
          END OF LS_ACCT_DRIVER.


    TYPES: BEGIN OF LS_RES2,
            AUDITTRAIL    TYPE CHAR32,
            CATEGORY      TYPE CHAR32,
            EMPLOYEE      TYPE CHAR32,
            ENTITY        TYPE CHAR32,
            RESOURCE_ACCT TYPE CHAR32,
            RPTCURRENCY   TYPE CHAR32,
            TIME          TYPE CHAR32,
            SIGNEDDATA    TYPE UJ_SDATA,
          END OF LS_RES2.


    DATA: BEGIN OF  LS_CTDATA,
            AUDITTRAIL    TYPE CHAR32,
            CATEGORY      TYPE CHAR32,
            EMPLOYEE      TYPE CHAR32,
            ENTITY        TYPE CHAR32,
            MEASURES      TYPE CHAR32,
            RESOURCE_ACCT TYPE CHAR32,
            RPTCURRENCY   TYPE CHAR32,
            TIME          TYPE CHAR32,
            SIGNEDDATA    TYPE UJ_SDATA,
          END OF LS_CTDATA.

    DATA: BEGIN OF LS_ENTITY,
            ID           TYPE CHAR32,
            CURRENCY     TYPE CHAR32,
            REG_CURRENCY TYPE CHAR3,
            COMPANY_CODE TYPE CHAR32,
            PROFIT_CENTRE TYPE CHAR20,
            ECCSTATUS_ID  TYPE CHAR2,
          END OF LS_ENTITY.

    DATA: BEGIN OF LS_EMPLOYEE,
            ID            TYPE CHAR32,
            RESOURCE_TYPE TYPE CHAR32,
            ITSS_CC       TYPE CHAR32,
            EMPL_STATUS_ID TYPE CHAR1,
          END OF LS_EMPLOYEE.

    DATA:    LT_TIMESCOPE      LIKE SORTED TABLE OF LS_TIME WITH UNIQUE KEY ID,
             LT_TIMESCOPE_PLUS LIKE STANDARD TABLE OF LS_TIME, "LIKE SORTED TABLE OF LS_TIME WITH UNIQUE KEY ID,
             LS_TIME2          LIKE LINE OF LT_TIMESCOPE,
             "LT_SALARY         LIKE STANDARD TABLE OF LS_SALARY, "THIS WILL CONTAIN THE SUM OF THE SALARY
             LT_EMPLOYEE       LIKE STANDARD TABLE OF LS_EMPLOYEE,
             LT_RESOURCE_TX    LIKE SORTED TABLE OF LS_RESOURCE WITH UNIQUE KEY
                AUDITTRAIL CATEGORY EMPLOYEE ENTITY RESOURCE_ACCT RPTCURRENCY TIME,
*             LT_RESOURCE_FLG   LIKE STANDARD TABLE OF LS_RESOURCE,
             LT_RESOURCE_FLG    LIKE SORTED TABLE OF LS_RESOURCE WITH UNIQUE KEY
                AUDITTRAIL CATEGORY EMPLOYEE ENTITY RESOURCE_ACCT RPTCURRENCY TIME,
             LT_RESOURCE_FLG2    LIKE SORTED TABLE OF LS_RESOURCE WITH UNIQUE KEY
                AUDITTRAIL CATEGORY EMPLOYEE ENTITY RESOURCE_ACCT RPTCURRENCY TIME,
             LT_RESOURCE_DATA  LIKE STANDARD TABLE OF LS_RESOURCE,
             LT_RESOURCE_DRV   LIKE STANDARD TABLE OF LS_RESOURCE,
             LT_CATEGORY       LIKE SORTED TABLE OF LS_CATEGORY WITH UNIQUE KEY ID,
             LT_ENTITY         LIKE STANDARD TABLE OF LS_ENTITY,
             LT_ENTITY_DRV     LIKE STANDARD TABLE OF LS_ENTITY,
             LT_DRIVER_LC      LIKE STANDARD TABLE OF LS_FX_ROW,
             LT_RP420_LC       LIKE STANDARD TABLE OF LS_FX_ROW,
             LT_ACCT_DRIVER    TYPE STANDARD TABLE OF LS_ACCT_DRIVER,
             WA_ACCT_DRIVER    TYPE LS_ACCT_DRIVER,
             LT_ASPACME_CONDRVR TYPE STANDARD TABLE OF LS_ACCT_DRIVER, "CHANGE3
             LO_TOOLS          TYPE REF TO ZCL_BPC_TOOLS_CLASSIC, "INSTANCE REFERENCE
             LT_SEL            TYPE UJ0_T_SEL,
             LS_SEL            TYPE UJ0_S_SEL,
             LS_DATA           TYPE UJA_S_DIM_MEMBER,
             L_LOG             TYPE STRING,
             LS_MESSAGE        TYPE UJ0_S_MESSAGE,
             COUNTER           TYPE INTEGER   ,
             ENT_CNTR          TYPE INTEGER   ,
             EMP_CNTR          TYPE INTEGER   ,
             WA_MEMBER         TYPE UJ_DIM_MEMBER,
             WA_MEMBER2         TYPE UJ_DIM_MEMBER,
             WA_MEMBERX        TYPE UJ_DIM_MEMBER,
             VAR_ORIGAMOUNT    TYPE UJ_SDATA,
             ORIG_AUDITTRAIL   TYPE UJ_DIM_MEMBER,
             ORIG_CATEGORY     TYPE UJ_DIM_MEMBER,
             ORIG_EMPLOYEE     TYPE UJ_DIM_MEMBER,
             ORIG_ENTITY       TYPE UJ_DIM_MEMBER,
             ORIG_ACCOUNT      TYPE UJ_DIM_MEMBER,
             ORIG_CURRENCY     TYPE UJ_DIM_MEMBER,
             ORIG_TIME         TYPE UJ_DIM_MEMBER,
             CURR_PERIOD       TYPE STRING,
             CURRENT_PERIOD    TYPE UJ_DIM_MEMBER,
             TIME_STRING       TYPE STRING,
             TIME_STRING1      TYPE STRING,
             TIME_STRING2      TYPE STRING,
             TIME_STRING3      TYPE STRING,
             TIME_STRING10     TYPE UJ_DIM_MEMBER,
             TIME_STRING11     TYPE UJ_DIM_MEMBER,
             TIME_STRING10_PLUS   TYPE UJ_DIM_MEMBER,
             TIME_STRING11_PLUS   TYPE UJ_DIM_MEMBER,
             EMP_COMPANY       TYPE STRING,
             TIME_STRINGME     TYPE STRING,
             ITIMEID           TYPE I,
             IMONTH            TYPE N LENGTH 2,
             LT_SALARYPACKAGE  TYPE UJA_T_DIM_MEMBER, "CONTAINS CHILDREN OF TOTAL SALARY PACKAGE
             LT_TOTAL_MONTHLYVALUES TYPE UJA_T_DIM_MEMBER,
             LT_TOTAL_STATACCT     TYPE UJA_T_DIM_MEMBER,
             LT_TOTAL_ITS      TYPE UJA_T_DIM_MEMBER, "CONTAINS CHILDREN OF TOTAL_ITS
             LT_ITSSROLES      TYPE UJA_T_DIM_MEMBER, "CONTAINS CHILDREN OF TOTAL_ITSSROLES
             LT_ITSSROLESROW   TYPE UJA_T_DIM_MEMBER, "CONTAINS CHILDREN OF TOTAL_ITSSROLES
             WA_SALARYPACKAGE  TYPE UJ_DIM_MEMBER, "LIKE LINE OF LT_SALARYPACKAGE,.
             LT_PENSIONALLOW   TYPE UJA_T_DIM_MEMBER, "CONTAINS CHILDREN OF TOTAL PENSIONALLOW
             LT_NONPENSIONALL  TYPE UJA_T_DIM_MEMBER, "CONTAINS CHILDREN OF TOTAL NONPENSIONALL
             LT_PENSIONSALARY  TYPE UJA_T_DIM_MEMBER, "CONTAINS CHILDREN OF TOTAL_PENSIONSALARY
             LT_NIABLESALARY   TYPE UJA_T_DIM_MEMBER, "CONTAINS CHILDREN OF TOTAL_NIABLESALARY
             WA_PENSIONALLOW   TYPE UJ_DIM_MEMBER, "LIKE LINE OF LT_PENSIONALLOW,.
             LT_TOTAL_ROLES    TYPE UJA_T_DIM_MEMBER, "EMPLOYEES UNDER TOTAL_ROLES
             LT_PROFCTR_CHILD   TYPE UJA_T_DIM_MEMBER, "CHILDREN OF PROFITCENTRE'S FROM ASPAC OR ME CONT DRIVERS
             LT_TOTAL_EMPLOYEES    TYPE UJA_T_DIM_MEMBER, "EMPLOYEES UNDER TOTAL_EMPLOYEES
             LT_EMP_ASPACME      TYPE UJA_T_DIM_MEMBER, "ASPAC OR ME EMP FROM TX DATA
             VAR_SALARY        TYPE UJ_SDATA,
             VAR_PENSIONALLOW  TYPE UJ_SDATA,
             VAR_NONPENSIONALL TYPE UJ_SDATA,
             LOWER_LIM         TYPE UJ_SDATA,
             UPPER_LIM         TYPE UJ_SDATA,
             NI_PERC           TYPE UJ_SDATA,
             VAR_PENSIONSALARY TYPE UJ_SDATA,
             VAR_NIABLESALARY  TYPE UJ_SDATA,
             VAR_ITSS_RATES    TYPE UJ_SDATA,
             INP_CAPPED        TYPE UJ_SDATA,
             VAR_ITAX          TYPE UJ_SDATA,
             VAR_ITAX_ROW      TYPE UJ_SDATA,
             Y1_RP020013       TYPE UJ_SDATA,
             Y2_RP020013       TYPE UJ_SDATA,
             ASPAC401          TYPE UJ_SDATA,
             ASPAC901          TYPE UJ_SDATA,
             ITSS_FLAG         TYPE CHAR1,
             EMPLOYEE_FLAG     TYPE CHAR1,
             ROLES_FLAG        TYPE CHAR1,
             VAR_RP000425       TYPE UJ_SDATA,
             VAR_ITSS_RATES_ROW    TYPE UJ_SDATA,
             IMPORT_PERIOD      TYPE UJ_DIM_MEMBER ,
             EXPORT_PERIOD      TYPE UJ_DIM_MEMBER ,
             PROFIT_CENTRE      TYPE UJ_DIM_MEMBER ,
             ENTITY_DRIVER      TYPE UJ_DIM_MEMBER ,
             ACCOUNT_DRIVER     TYPE UJ_DIM_MEMBER ,
             ITSS_CC            TYPE UJ_DIM_MEMBER ,
             ECCSTATUS_ID       TYPE STRING,
             EMPL_STATUS_ID     TYPE STRING,
             YEAR1_APR          TYPE STRING,
             YEAR2_APR          TYPE STRING,
             GATE1              TYPE CHAR1,
             GATE2              TYPE CHAR1,
             GATE3              TYPE CHAR1,"CHANGE3
             GATE4              TYPE CHAR1,"CHANGE3
             GATE5              TYPE CHAR1,"CHANGE3
             GATE6              TYPE CHAR1,"CHANGE3
             GATE7              TYPE CHAR1."CHANGE3

FIELD-SYMBOLS : <LS_RESOURCE> TYPE LS_RES2 ,
               <LS_RESOURCE_FLG> TYPE LS_RES2 .

    DATA: FLAG1         TYPE CHAR1,
          RESOURCE_TYPE TYPE C LENGTH 1,
          REGION        TYPE CHAR32,
          LOCCURR       TYPE CHAR32,
          COMPANY       TYPE CHAR32,
          FACTOR1       TYPE UJ_SDATA,
          FACTOR2       TYPE UJ_SDATA,
          FACTOR3       TYPE UJ_SDATA,
          FACTOR4       TYPE UJ_SDATA,
          FACTOR5       TYPE UJ_SDATA,
          FACTOR6       TYPE UJ_SDATA,
          FACTOR7       TYPE UJ_SDATA,
          FACTOR8       TYPE UJ_SDATA,
          FACTOR9       TYPE UJ_SDATA,
          FACTOR10      TYPE UJ_SDATA,
          FACTOR11      TYPE UJ_SDATA,
          FACTOR12      TYPE UJ_SDATA,
          FACTOR13      TYPE UJ_SDATA,
          FACTOR14      TYPE UJ_SDATA,
          FACTOR15      TYPE UJ_SDATA,
          FACTOR16      TYPE UJ_SDATA,
          FACTOR17      TYPE UJ_SDATA,
          FACTOR18      TYPE UJ_SDATA,
          FACTOR19      TYPE UJ_SDATA,
          FACTOR20      TYPE UJ_SDATA,
          FTE_408       TYPE UJ_SDATA.

"CHANGE3 START
IF SUB_ID = 'NONE'.
    CLEAR: LT_ASPACME_CONDRVR , WA_ACCT_DRIVER .
    WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000420'.
    WA_ACCT_DRIVER-TARGET_ACCT =  'RP000408'.
    APPEND WA_ACCT_DRIVER TO LT_ASPACME_CONDRVR.
"ASPAC DRIVERS
    WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000210'.
    WA_ACCT_DRIVER-TARGET_ACCT =  'RP000408'.
    APPEND WA_ACCT_DRIVER TO LT_ASPACME_CONDRVR.
    WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000211'.
    WA_ACCT_DRIVER-TARGET_ACCT =  'RP000408'.
    APPEND WA_ACCT_DRIVER TO LT_ASPACME_CONDRVR.
    WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000212'.
    WA_ACCT_DRIVER-TARGET_ACCT =  'RP000408'.
    APPEND WA_ACCT_DRIVER TO LT_ASPACME_CONDRVR.
    WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000401'.
    WA_ACCT_DRIVER-TARGET_ACCT =  'RP000408'.
    APPEND WA_ACCT_DRIVER TO LT_ASPACME_CONDRVR.
    WA_ACCT_DRIVER-DRIVER_ACCT =  'RP900001'.
    WA_ACCT_DRIVER-TARGET_ACCT =  'RP000408'.
    APPEND WA_ACCT_DRIVER TO LT_ASPACME_CONDRVR.
"ME DRIVERS
    WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000136'.
    WA_ACCT_DRIVER-TARGET_ACCT =  'RP000408'.
    APPEND WA_ACCT_DRIVER TO LT_ASPACME_CONDRVR.
    WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000100'.
    WA_ACCT_DRIVER-TARGET_ACCT =  'RP000408'.
    APPEND WA_ACCT_DRIVER TO LT_ASPACME_CONDRVR.
    WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000101'.
    WA_ACCT_DRIVER-TARGET_ACCT =  'RP000408'.
    APPEND WA_ACCT_DRIVER TO LT_ASPACME_CONDRVR.
    WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000102'.
    WA_ACCT_DRIVER-TARGET_ACCT =  'RP000408'.
    APPEND WA_ACCT_DRIVER TO LT_ASPACME_CONDRVR.
    WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000103'.
    WA_ACCT_DRIVER-TARGET_ACCT =  'RP000408'.
    APPEND WA_ACCT_DRIVER TO LT_ASPACME_CONDRVR.
    WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000104'.
    WA_ACCT_DRIVER-TARGET_ACCT =  'RP000408'.
    APPEND WA_ACCT_DRIVER TO LT_ASPACME_CONDRVR.
    WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000105'.
    WA_ACCT_DRIVER-TARGET_ACCT =  'RP000408'.
    APPEND WA_ACCT_DRIVER TO LT_ASPACME_CONDRVR.
    WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000106'.
    WA_ACCT_DRIVER-TARGET_ACCT =  'RP000408'.
    APPEND WA_ACCT_DRIVER TO LT_ASPACME_CONDRVR.
    WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000107'.
    WA_ACCT_DRIVER-TARGET_ACCT =  'RP000408'.
    APPEND WA_ACCT_DRIVER TO LT_ASPACME_CONDRVR.

SORT LT_ASPACME_CONDRVR BY DRIVER_ACCT.

    ENDIF.

    CLEAR LT_RESOURCE_FLG2.
"CHANGE3 END


IF SUB_ID = 'DRVRS' .
"BELOW IS USED WHAN THE ADMIN INPUT SCHEDULE IS USED
"POPULATE THE LT_ACCT_DRIVER
WA_ACCT_DRIVER-DRIVER_ACCT =  'RP900001'.
WA_ACCT_DRIVER-TARGET_ACCT =  'RP900001'.
APPEND WA_ACCT_DRIVER TO LT_ACCT_DRIVER.
WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000401'.
WA_ACCT_DRIVER-TARGET_ACCT =  'RP000401'.
APPEND WA_ACCT_DRIVER TO LT_ACCT_DRIVER.
WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000010'.
WA_ACCT_DRIVER-TARGET_ACCT =  'RP900006'.
APPEND WA_ACCT_DRIVER TO LT_ACCT_DRIVER.
WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000011'.
WA_ACCT_DRIVER-TARGET_ACCT =  'RP900006'.
APPEND WA_ACCT_DRIVER TO LT_ACCT_DRIVER.
WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000012'.
WA_ACCT_DRIVER-TARGET_ACCT =  'RP900006'.
APPEND WA_ACCT_DRIVER TO LT_ACCT_DRIVER.
WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000013'.
WA_ACCT_DRIVER-TARGET_ACCT =  'RP900006'.
APPEND WA_ACCT_DRIVER TO LT_ACCT_DRIVER.
WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000014'.
WA_ACCT_DRIVER-TARGET_ACCT =  'RP900006'.
APPEND WA_ACCT_DRIVER TO LT_ACCT_DRIVER.

WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000015'.
WA_ACCT_DRIVER-TARGET_ACCT =  'RP900005'.
APPEND WA_ACCT_DRIVER TO LT_ACCT_DRIVER.
WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000016'.
WA_ACCT_DRIVER-TARGET_ACCT =  'RP900005'.
APPEND WA_ACCT_DRIVER TO LT_ACCT_DRIVER.
WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000017'.
WA_ACCT_DRIVER-TARGET_ACCT =  'RP900005'.
APPEND WA_ACCT_DRIVER TO LT_ACCT_DRIVER.
WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000018'.
WA_ACCT_DRIVER-TARGET_ACCT =  'RP900005'.
APPEND WA_ACCT_DRIVER TO LT_ACCT_DRIVER.
WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000019'.
WA_ACCT_DRIVER-TARGET_ACCT =  'RP900005'.
APPEND WA_ACCT_DRIVER TO LT_ACCT_DRIVER.
***WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000420'.
***WA_ACCT_DRIVER-TARGET_ACCT =  'RP010001'. "'AC310000P'.
***APPEND WA_ACCT_DRIVER TO LT_ACCT_DRIVER.
***WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000025'.
***WA_ACCT_DRIVER-TARGET_ACCT =  'RP020007'.
***APPEND WA_ACCT_DRIVER TO LT_ACCT_DRIVER.
***WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000001'.
***WA_ACCT_DRIVER-TARGET_ACCT =  'RP020011'.
***APPEND WA_ACCT_DRIVER TO LT_ACCT_DRIVER.
***WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000002'.
***WA_ACCT_DRIVER-TARGET_ACCT =  'RP020010'.
***APPEND WA_ACCT_DRIVER TO LT_ACCT_DRIVER.
***WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000003'.
***WA_ACCT_DRIVER-TARGET_ACCT =  'RP020010'.
***APPEND WA_ACCT_DRIVER TO LT_ACCT_DRIVER.
WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000136'.
WA_ACCT_DRIVER-TARGET_ACCT =  'RP010001'. "'AC710170'.
APPEND WA_ACCT_DRIVER TO LT_ACCT_DRIVER.
WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000100'.
WA_ACCT_DRIVER-TARGET_ACCT =  'RP900006'. "'RP010001'.
APPEND WA_ACCT_DRIVER TO LT_ACCT_DRIVER.
WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000101'.
WA_ACCT_DRIVER-TARGET_ACCT =  'RP900006'. "'RP010001'.
APPEND WA_ACCT_DRIVER TO LT_ACCT_DRIVER.
WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000102'.
WA_ACCT_DRIVER-TARGET_ACCT =  'RP900006'. "'RP010001'.
APPEND WA_ACCT_DRIVER TO LT_ACCT_DRIVER.
WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000103'.
WA_ACCT_DRIVER-TARGET_ACCT =  'RP900006'. "'RP010001'.
APPEND WA_ACCT_DRIVER TO LT_ACCT_DRIVER.
WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000104'.
WA_ACCT_DRIVER-TARGET_ACCT =  'RP900006'. "'RP010001'.
APPEND WA_ACCT_DRIVER TO LT_ACCT_DRIVER.
WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000105'.
WA_ACCT_DRIVER-TARGET_ACCT = 'RP900006'. "'RP010001'.
APPEND WA_ACCT_DRIVER TO LT_ACCT_DRIVER.
WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000106'.
WA_ACCT_DRIVER-TARGET_ACCT =  'RP900006'. "'RP010001'.
APPEND WA_ACCT_DRIVER TO LT_ACCT_DRIVER.
WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000107'.
WA_ACCT_DRIVER-TARGET_ACCT =  'RP900006'. "'RP010001'.
APPEND WA_ACCT_DRIVER TO LT_ACCT_DRIVER.
ENDIF.

IF SUB_ID = 'ITS_R'.
WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000430'. "ITS RATE CARD
WA_ACCT_DRIVER-TARGET_ACCT =  'RP000431'. "ITS DAYS
APPEND WA_ACCT_DRIVER TO LT_ACCT_DRIVER.
ENDIF.

SORT LT_ACCT_DRIVER BY DRIVER_ACCT.

*******************************************************************************
    "CHECK IT_CV FOR CURRENT VIEW THIS WILL CONTAIN CATEGORY WITH ONLY 1 VALUE.
    " READ IT_CV values
****** SUB SPECIFIC VARIABLES EXTENDED HERE ***********************************

    DATA: FLOWS    TYPE UJA_T_DIM_MEMBER,
          FLOWS2   TYPE UJA_T_DIM_MEMBER, "additional flows read
          LT_FLOWS TYPE UJA_T_DIM_MEMBER,
          ACCOUNTS TYPE UJA_T_DIM_MEMBER.

    DATA: CV_AUDITTRAIL    TYPE UJK_S_CV,
          CV_RPTCURRENCY   TYPE UJK_S_CV,
          CV_CATEGORY      TYPE UJK_S_CV,
          CV_FLOW          TYPE UJK_S_CV,
          CV_ENTITY        TYPE UJK_S_CV,
          CV_EMPLOYEE      TYPE UJK_S_CV,
          CV_ACCOUNT       TYPE UJK_S_CV,
          CV_TIME          TYPE UJK_S_CV,
          LT_CV_EMPLOYEE   TYPE UJA_T_DIM_MEMBER,
          LT_CV_ENTITY     TYPE UJA_T_DIM_MEMBER,
          LT_CV_ACCOUNT    TYPE UJA_T_DIM_MEMBER,
          LT_TARGET_ACCT   TYPE UJA_T_DIM_MEMBER,
          LT_TARGET_EMPLEE   TYPE UJA_T_DIM_MEMBER,
          LT_TARGET_ENTITY   TYPE UJA_T_DIM_MEMBER,
          WORKING_CATEGORY TYPE STRING , "UJ_DIM_MEMBER, "BUDGET OR WFORECAST
          CATEGORY_YEAR    TYPE CHAR4,
          CATEGORY_YEAR2   TYPE CHAR4,
          YEAR2_INP        TYPE UJ_DIM_MEMBER,
          YEAR_INP         TYPE UJ_DIM_MEMBER.

*** SET TOOLS INSTANCE *****
    CREATE OBJECT LO_TOOLS
      EXPORTING
        I_ENV_ID = I_APPSET_ID
        I_MOD_ID = I_APPL_ID
        I_T_CV = IT_CV.

    READ TABLE IT_CV INTO CV_CATEGORY WITH TABLE KEY DIM_UPPER_CASE = 'CATEGORY'.
**** Cant process two categoroes simultaneously due to time dim restrictions in WFORECAST and BUDGET
    LOOP AT CV_CATEGORY-MEMBER INTO WA_MEMBER.
      IF SY-TABIX > 1.
        CLEAR: ET_MESSAGE, L_LOG.
        L_LOG = 'DO NOT SEND MULTIPLE CATEGORIES SIMULTANEOUSLY! RESTRICT IN PACKAGE OR IN INPUT SCHEDULE.'.
        CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).
        EXIT.
      ENDIF.
    ENDLOOP.
    WORKING_CATEGORY = WA_MEMBER. "either WFORECAST OR BUDGET

    "READ ENTITY THAT ARE IN THE IT_CV TABLE
    READ TABLE IT_CV INTO CV_ENTITY WITH TABLE KEY DIM_UPPER_CASE = 'ENTITY'.
"GATE1 AND GATE2 IS FOR ME / ASPAC GLOBAL
    CLEAR: GATE1 , GATE2 , ENT_CNTR , GATE2, GATE3, GATE4 , GATE5 , EMP_CNTR .

    LOOP AT CV_ENTITY-MEMBER INTO WA_MEMBER.
      APPEND WA_MEMBER TO LT_CV_ENTITY.
      IF WA_MEMBER(2) = 'PC' .
        COUNTER = STRLEN( WA_MEMBER ).
        COUNTER = COUNTER - 2 . "BS
        IF WA_MEMBER+COUNTER(2) = 'BS' .
          PROFIT_CENTRE = WA_MEMBER(COUNTER).
          ENT_CNTR = ENT_CNTR + 1.
          ENDIF.
        ENDIF.
    ENDLOOP.

    IF ENT_CNTR = 1 ."DONT FORGET full calc will have loads of PC{}BS
      GATE1 = 1.
      ENDIF.

    "READ EMPLOYEE THAT ARE IN THE IT_CV TABLE
    READ TABLE IT_CV INTO CV_EMPLOYEE WITH TABLE KEY DIM_UPPER_CASE = 'EMPLOYEE'.
    "DESCRIBE TABLE CV_EMPLOYEE-MEMBER

    LOOP AT CV_EMPLOYEE-MEMBER INTO WA_MEMBER.
      APPEND WA_MEMBER TO LT_CV_EMPLOYEE.
      IF WA_MEMBER = 'NO_EMPLOYEE'.
        GATE2 = 1.
        ENDIF.
    ENDLOOP.


"CHANGE3 START
DESCRIBE TABLE LT_CV_EMPLOYEE LINES EMP_CNTR.

READ TABLE IT_CV INTO CV_AUDITTRAIL WITH TABLE KEY DIM_UPPER_CASE = 'AUDITTRAIL'.
    LOOP AT CV_AUDITTRAIL-MEMBER INTO WA_MEMBER.
      IF SY-TABIX > 1.
        GATE3 = 0.
        ELSEIF WA_MEMBER = 'INPUT'.
          GATE3 = 1.
      ENDIF.
    ENDLOOP.

READ TABLE IT_CV INTO CV_RPTCURRENCY WITH TABLE KEY DIM_UPPER_CASE = 'RPTCURRENCY'.
    LOOP AT CV_RPTCURRENCY-MEMBER INTO WA_MEMBER.
      IF SY-TABIX > 1.
        GATE4 = 0.
        ELSEIF WA_MEMBER = 'LC'.
          GATE4 = 1.
      ENDIF.
    ENDLOOP.


IF SUB_ID = 'NONE' AND GATE1 = 1 AND GATE2 = 1 AND EMP_CNTR = 1
  AND GATE3 = 1 AND GATE4 = 1.
  "CHECK ACCOUNTS ARE PART OF LT_ASPACME_CONDRVR
    READ TABLE IT_CV INTO CV_ACCOUNT WITH TABLE KEY DIM_UPPER_CASE = 'RESOURCE_ACCT'.

    LOOP AT CV_ACCOUNT-MEMBER INTO WA_MEMBER.
      READ TABLE LT_ASPACME_CONDRVR
      TRANSPORTING NO FIELDS
      WITH KEY
      DRIVER_ACCT =  WA_MEMBER
      BINARY SEARCH.
      IF SY-SUBRC = 0.
        GATE5 = 1.
        ELSE.
          GATE5 = 0.
        ENDIF.
    ENDLOOP.






    IF GATE5 = 1.
***** "GET CHILDREN OF PRFITCENTRE *********************************
    CLEAR: LS_SEL , LT_SEL.
    LS_SEL-DIMENSION = 'ENTITY'.

    LT_PROFCTR_CHILD = LO_TOOLS->GET_PARENT_MD( EXPORTING
      PARENT_MBR = PROFIT_CENTRE
      I_DIMENSION_ID = LS_SEL-DIMENSION ).
    SORT LT_PROFCTR_CHILD .
**********************************************************************
    ENDIF.

  ENDIF.
"CHANGE3 END


******************* GET THE COMMON "VARIABLES" *********
*  SUCH AS TIMESCOPE - FORECAST where PROPERY CURRMONTH = C,F in TIME dimension
*  BUDGET - where PROPERY CURRMONTH = B in TIME definition
**************************************************************
    DATA: MASTER_DATA   TYPE REF TO DATA.
    FIELD-SYMBOLS: <DIMENSION_DATA> TYPE STANDARD TABLE.

***** GET TIME REFERENCE *************************************
    LO_TOOLS->TIME( EXPORTING
       CATEGORY       = WORKING_CATEGORY
     IMPORTING
       TIMESCOPE      = LT_TIMESCOPE
*    FIRST_PERIOD   =
*    LAST_PERIOD    =
       CURRENT_PERIOD = CURRENT_PERIOD ).

IF WORKING_CATEGORY = 'WFORECAST'.
    CURR_PERIOD = CURRENT_PERIOD ."-DIMENSION .
    CURR_PERIOD =  CURR_PERIOD(4) && CURR_PERIOD+5(2) && '00'.
    CONDENSE CURR_PERIOD NO-GAPS.

    ENDIF.

IF WORKING_CATEGORY = 'BUDGET'.
    READ TABLE LT_TIMESCOPE INDEX 1 INTO LS_TIME.
    CURRENT_PERIOD = LS_TIME-ID. "ALWAYS FIRST PERIOD
    CURR_PERIOD = CURRENT_PERIOD ."-DIMENSION .
    CURR_PERIOD =  CURR_PERIOD(4) && CURR_PERIOD+5(2) && '00'.
    CONDENSE CURR_PERIOD NO-GAPS.
    ENDIF.


***** GET CATEGORY REFERENCE  *************************************
    CLEAR: LS_SEL , LT_SEL.
    LS_SEL-DIMENSION = 'CATEGORY'.

    LO_TOOLS->READ_MASTER_DATA(
      EXPORTING
        I_DIMENSION_ID   = LS_SEL-DIMENSION
      IMPORTING
        OUTPUT_R_DATA = MASTER_DATA
      CHANGING
        LT_SEL        = LT_SEL ).

    ASSIGN MASTER_DATA->* TO <DIMENSION_DATA>.
    MOVE-CORRESPONDING <DIMENSION_DATA> TO LT_CATEGORY.

********************************************************************
***** GET ENTITY REFERENCE  *************************************
    CLEAR: LS_SEL , LT_SEL.
    LS_SEL-DIMENSION = 'ENTITY'.
***"TRIM DOWN REFERENCE TABLE AS SMALL AS POSSIBLE
***   LOOP AT LT_CV_ENTITY INTO WA_MEMBER.
***      LS_SEL-DIMENSION = 'ENTITY'.
***      LS_SEL-ATTRIBUTE = 'ID'.
***      LS_SEL-SIGN = 'I'.
***      LS_SEL-OPTION = 'EQ'.
***      LS_SEL-LOW = WA_MEMBER.
***      APPEND LS_SEL TO LT_SEL.
***    ENDLOOP.

IF GATE5 = 1 AND SUB_ID = 'NONE'.
  "LT_PROFCTR_CHILD
   LOOP AT LT_PROFCTR_CHILD INTO WA_MEMBER.
      LS_SEL-DIMENSION = 'ENTITY'.
      LS_SEL-ATTRIBUTE = 'ID'.
      LS_SEL-SIGN = 'I'.
      LS_SEL-OPTION = 'EQ'.
      LS_SEL-LOW = WA_MEMBER.
      APPEND LS_SEL TO LT_SEL.
    ENDLOOP.
  ENDIF.

    LO_TOOLS->READ_MASTER_DATA(
      EXPORTING
        I_DIMENSION_ID   = LS_SEL-DIMENSION
      IMPORTING
        OUTPUT_R_DATA = MASTER_DATA
      CHANGING
        LT_SEL        = LT_SEL ).

    ASSIGN MASTER_DATA->* TO <DIMENSION_DATA>.
    MOVE-CORRESPONDING <DIMENSION_DATA> TO LT_ENTITY.
    SORT LT_ENTITY BY ID.


CLEAR: LT_TARGET_ACCT , COUNTER.
"CHECKS IF DATA IS BEING CALLED FROM DRIVER INPUT SCHEDULE
DESCRIBE TABLE LT_CV_ENTITY LINES COUNTER.
IF COUNTER = 1.
  READ TABLE LT_CV_ENTITY INDEX COUNTER INTO WA_MEMBER.
  IF SY-SUBRC = 0.
    IF WA_MEMBER(15) = 'NO_PROFITCENTRE'.
      REGION = WA_MEMBER.
      REGION = REGION+15(2).
      CLEAR: LS_SEL , LT_SEL.
      CASE REGION.

        WHEN 'UK'.
          FACTOR1 = 1.
          LS_SEL-DIMENSION = 'ENTITY'.
          LS_SEL-ATTRIBUTE = 'REG_CURRENCY'.
          LS_SEL-SIGN = 'I'.
          LS_SEL-OPTION = 'EQ'.
          LS_SEL-LOW = 'GBP'.
          APPEND LS_SEL TO LT_SEL.
        WHEN 'ME'.
          FACTOR1 = 1.
          LS_SEL-DIMENSION = 'ENTITY'.
          LS_SEL-ATTRIBUTE = 'REG_CURRENCY'.
          LS_SEL-SIGN = 'I'.
          LS_SEL-OPTION = 'EQ'.
          LS_SEL-LOW = 'AED'.
          APPEND LS_SEL TO LT_SEL.
        WHEN 'AS'.
          FACTOR1 = 1.
          LS_SEL-DIMENSION = 'ENTITY'.
          LS_SEL-ATTRIBUTE = 'REG_CURRENCY'.
          LS_SEL-SIGN = 'I'.
          LS_SEL-OPTION = 'EQ'.
          LS_SEL-LOW = 'AUD'.
          APPEND LS_SEL TO LT_SEL.
        WHEN 'EU'.
          FACTOR1 = 0.

        WHEN OTHERS.
          FACTOR1 = 0.

       ENDCASE.

       IF FACTOR1 = 1.
       "READ ENTITY DIMENSION FOR THE SPECIFIED REGIONAL CURRENCY.
        LS_SEL-DIMENSION = 'ENTITY'.
        LS_SEL-ATTRIBUTE = 'CALC'.
        LS_SEL-SIGN = 'I'.
        LS_SEL-OPTION = 'EQ'.
        LS_SEL-LOW = 'N'.
        APPEND LS_SEL TO LT_SEL.

        LO_TOOLS->READ_MASTER_DATA(
          EXPORTING
            I_DIMENSION_ID   = LS_SEL-DIMENSION
          IMPORTING
            OUTPUT_R_DATA = MASTER_DATA
          CHANGING
            LT_SEL        = LT_SEL ).

        ASSIGN MASTER_DATA->* TO <DIMENSION_DATA>.
        MOVE-CORRESPONDING <DIMENSION_DATA> TO LT_ENTITY_DRV.
        SORT LT_ENTITY_DRV BY ID.

        "READ RESOURCE_ACCT THAT ARE IN THE IT_CV TABLE
        READ TABLE IT_CV INTO CV_ACCOUNT WITH TABLE KEY DIM_UPPER_CASE = 'RESOURCE_ACCT'.

        CLEAR LT_TARGET_ACCT.
        LOOP AT CV_ACCOUNT-MEMBER INTO WA_MEMBER.
          APPEND WA_MEMBER TO LT_CV_ACCOUNT.
          "CHECK LT_CV_ACCOUNT IF IT CONTAINS ACCOUNT DRIVERS !!!
          READ TABLE LT_ACCT_DRIVER
          INTO WA_ACCT_DRIVER
          WITH KEY
          DRIVER_ACCT =  WA_MEMBER
          BINARY SEARCH.
          IF SY-SUBRC = 0.
            APPEND WA_ACCT_DRIVER-TARGET_ACCT TO LT_TARGET_ACCT.

            ENDIF.
        ENDLOOP.

        SORT LT_TARGET_ACCT.
        DELETE ADJACENT DUPLICATES FROM LT_TARGET_ACCT.

        ENDIF.

      ENDIF.
    ENDIF.
  ENDIF.

  IF GATE1 = 1 AND GATE2 = 1 AND FACTOR1 = 0.
    "SET FACTOR1 = 1
    FACTOR1 = 1 . "to enable filling of table LT_RESOURCE_DRV.
    "GET ENTITIES THAT USES THE PROFIT_CENTRE
    CLEAR: LS_SEL , LT_SEL.
    LS_SEL-DIMENSION = 'ENTITY'.
    LS_SEL-ATTRIBUTE = 'CALC'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = 'N'.
    APPEND LS_SEL TO LT_SEL.

    LS_SEL-DIMENSION = 'ENTITY'.
    LS_SEL-ATTRIBUTE = 'PROFIT_CENTRE'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = PROFIT_CENTRE. "without the BS
    APPEND LS_SEL TO LT_SEL.

    LO_TOOLS->READ_MASTER_DATA(
      EXPORTING
        I_DIMENSION_ID   = LS_SEL-DIMENSION
      IMPORTING
        OUTPUT_R_DATA = MASTER_DATA
      CHANGING
        LT_SEL        = LT_SEL ).

    ASSIGN MASTER_DATA->* TO <DIMENSION_DATA>.
    MOVE-CORRESPONDING <DIMENSION_DATA> TO LT_ENTITY_DRV.
    SORT LT_ENTITY_DRV BY ID.

    "READ RESOURCE_ACCT THAT ARE IN THE IT_CV TABLE
    READ TABLE IT_CV INTO CV_ACCOUNT WITH TABLE KEY DIM_UPPER_CASE = 'RESOURCE_ACCT'.

    CLEAR LT_TARGET_ACCT.
    LOOP AT CV_ACCOUNT-MEMBER INTO WA_MEMBER.
      APPEND WA_MEMBER TO LT_CV_ACCOUNT.
      "CHECK LT_CV_ACCOUNT IF IT CONTAINS ACCOUNT DRIVERS !!!
      READ TABLE LT_ACCT_DRIVER
      INTO WA_ACCT_DRIVER
      WITH KEY
      DRIVER_ACCT =  WA_MEMBER
      BINARY SEARCH.
      IF SY-SUBRC = 0.
        APPEND WA_ACCT_DRIVER-TARGET_ACCT TO LT_TARGET_ACCT.

        ENDIF.
    ENDLOOP.

    SORT LT_TARGET_ACCT.
    DELETE ADJACENT DUPLICATES FROM LT_TARGET_ACCT.

    ENDIF.

************************************************************************
***** GET CHILDREN OF TOTAL_SALARYPACKAGE  *****************************
    CLEAR: LS_SEL , LT_SEL.
    LS_SEL-DIMENSION = 'RESOURCE_ACCT'.

    LT_SALARYPACKAGE = LO_TOOLS->GET_PARENT_MD( EXPORTING
      PARENT_MBR = 'TOTAL_SALARYPACKAGE'
      I_DIMENSION_ID = LS_SEL-DIMENSION ).

    SORT LT_SALARYPACKAGE .
**********************************************************************
***** GET CHILDREN OF TOTAL_PENSIONALLOW *****************************
    CLEAR: LS_SEL , LT_SEL.
    LS_SEL-DIMENSION = 'RESOURCE_ACCT'.

    LT_PENSIONALLOW = LO_TOOLS->GET_PARENT_MD( EXPORTING
      PARENT_MBR = 'TOTAL_PENSIONALLOW'
      I_DIMENSION_ID = LS_SEL-DIMENSION ).

    SORT LT_PENSIONALLOW .
*******************************************************************
***** GET CHILDREN OF TOTAL_PENSIONSALARY**************************
    CLEAR: LS_SEL , LT_SEL.
    LS_SEL-DIMENSION = 'RESOURCE_ACCT'.

    LT_PENSIONSALARY = LO_TOOLS->GET_PARENT_MD( EXPORTING
      PARENT_MBR = 'TOTAL_PENSIONSALARY'
      I_DIMENSION_ID = LS_SEL-DIMENSION ).

    SORT LT_PENSIONSALARY .
*******************************************************************

***** GET CHILDREN OF TOTAL_NONPENSIONALL *****************************
    CLEAR: LS_SEL , LT_SEL.
    LS_SEL-DIMENSION = 'RESOURCE_ACCT'.

    LT_NONPENSIONALL = LO_TOOLS->GET_PARENT_MD( EXPORTING
      PARENT_MBR = 'TOTAL_NONPENSIONALL'
      I_DIMENSION_ID = LS_SEL-DIMENSION ).

    SORT LT_NONPENSIONALL .
*******************************************************************

***** GET CHILDREN OF 'TOTAL_NIABLESALARY'*************************
    CLEAR: LS_SEL , LT_SEL.
    LS_SEL-DIMENSION = 'RESOURCE_ACCT'.

    LT_NIABLESALARY = LO_TOOLS->GET_PARENT_MD( EXPORTING
      PARENT_MBR = 'TOTAL_NIABLESALARY'
      I_DIMENSION_ID = LS_SEL-DIMENSION ).

    SORT LT_NIABLESALARY .
*******************************************************************
***** GET CHILDREN OF TOTAL_MONTHLYVALUES *****************************
    CLEAR: LS_SEL , LT_SEL.
    LS_SEL-DIMENSION = 'RESOURCE_ACCT'.

    LT_TOTAL_MONTHLYVALUES = LO_TOOLS->GET_PARENT_MD( EXPORTING
      PARENT_MBR = 'TOTAL_MONTHLYVALUES'
      I_DIMENSION_ID = LS_SEL-DIMENSION ).

    SORT LT_TOTAL_MONTHLYVALUES .
**********************************************************************
***** GET CHILDREN OF TOTAL_STATACCT *********************************
    CLEAR: LS_SEL , LT_SEL.
    LS_SEL-DIMENSION = 'RESOURCE_ACCT'.

    LT_TOTAL_STATACCT = LO_TOOLS->GET_PARENT_MD( EXPORTING
      PARENT_MBR = 'TOTAL_STATACCT'
      I_DIMENSION_ID = LS_SEL-DIMENSION ).

    SORT LT_TOTAL_STATACCT .
**********************************************************************



"EMPLOYEE DIMENSION REFERENCES
***** GET CHILDREN OF 'TOTAL_ITROLES' *****************************
    CLEAR: LS_SEL , LT_SEL.
    LS_SEL-DIMENSION = 'EMPLOYEE'.

    LT_TOTAL_ITS = LO_TOOLS->GET_PARENT_MD( EXPORTING
      PARENT_MBR = 'TOTAL_ITROLES'
      I_DIMENSION_ID = LS_SEL-DIMENSION ).

    SORT LT_TOTAL_ITS.
*******************************************************************
***** GET CHILDREN OF 'TOTAL_ROLES' *******************************
    CLEAR: LS_SEL , LT_SEL.
    LS_SEL-DIMENSION = 'EMPLOYEE'.

    LT_TOTAL_ROLES = LO_TOOLS->GET_PARENT_MD( EXPORTING
      PARENT_MBR = 'TOTAL_ROLES'
      I_DIMENSION_ID = LS_SEL-DIMENSION ).

    SORT LT_TOTAL_ROLES.
*********************************************************************
***** GET CHILDREN OF 'TOTAL_EMPLOYEES' *******************************
    CLEAR: LS_SEL , LT_SEL.
    LS_SEL-DIMENSION = 'EMPLOYEE'.

    LT_TOTAL_EMPLOYEES = LO_TOOLS->GET_PARENT_MD( EXPORTING
      PARENT_MBR = 'TOTAL_EMPLOYEES'
      I_DIMENSION_ID = LS_SEL-DIMENSION ).

    SORT LT_TOTAL_EMPLOYEES.
*********************************************************************
***** GET CHILDREN OF 'TOTAL_ITSSROLES' *****************************
    CLEAR: LS_SEL , LT_SEL.
    LS_SEL-DIMENSION = 'EMPLOYEE'.

    LT_ITSSROLES = LO_TOOLS->GET_PARENT_MD( EXPORTING
      PARENT_MBR = 'TOTAL_ITSSROLES'
      I_DIMENSION_ID = LS_SEL-DIMENSION ).

    SORT LT_ITSSROLES.

*******************************************************************

***** GET EMPLOYEE REFERENCE  *************************************
    CLEAR: LS_SEL , LT_SEL.
    LS_SEL-DIMENSION = 'EMPLOYEE'.

    "IF GATE5 = 1 AND SUB_ID = 'NONE'."CHANGE3 START
      CLEAR: LS_SEL , LT_SEL.
      LS_SEL-DIMENSION = 'EMPLOYEE'.

    "  ELSE. "CHANGE3 END
***    LOOP AT LT_CV_EMPLOYEE INTO WA_MEMBER.
***      LS_SEL-DIMENSION = 'EMPLOYEE'.
***      LS_SEL-ATTRIBUTE = 'ID'.
***      LS_SEL-SIGN = 'I'.
***      LS_SEL-OPTION = 'EQ'.
***      LS_SEL-LOW = WA_MEMBER.
***      APPEND LS_SEL TO LT_SEL.
***    ENDLOOP.

    "ENDIF.

    LO_TOOLS->READ_MASTER_DATA(
      EXPORTING
        I_DIMENSION_ID   = LS_SEL-DIMENSION
      IMPORTING
        OUTPUT_R_DATA = MASTER_DATA
      CHANGING
        LT_SEL        = LT_SEL ).

    ASSIGN MASTER_DATA->* TO <DIMENSION_DATA>.
    MOVE-CORRESPONDING <DIMENSION_DATA> TO LT_EMPLOYEE.
    SORT LT_EMPLOYEE BY ID.

********************************************************************


********************************************************************
    "GET CATEGORY YEAR FROM LT_CATEGORY
    READ TABLE LT_CATEGORY INTO LS_CATEGORY
    WITH KEY
    ID = WORKING_CATEGORY.
    IF SY-SUBRC = 0.
      CATEGORY_YEAR = LS_CATEGORY-YEAR.
      YEAR_INP   =   CATEGORY_YEAR && '.INP' .
      CATEGORY_YEAR2  =  CATEGORY_YEAR + 1.
      YEAR2_INP  =  CATEGORY_YEAR2 && '.INP' .
    ELSE.
      CLEAR: ET_MESSAGE, L_LOG.
      L_LOG = 'CATEGORY HAS NO YEAR MAINTAINED'.
      CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).
      EXIT.
    ENDIF.

**********************************************************************

CLEAR: FACTOR2 .
IF SUB_ID = 'ITS_R'.
  CLEAR: FACTOR2 .
"READ IT_CV FOR TIME FOR RATE_CARD
    READ TABLE IT_CV INTO CV_TIME WITH TABLE KEY DIM_UPPER_CASE = 'TIME'.
    LOOP AT CV_TIME-MEMBER INTO WA_MEMBER.
      IF SY-TABIX = 1 AND WA_MEMBER = YEAR_INP .
        FACTOR2 = 1.
        ELSE.
        FACTOR2 = 0.
      ENDIF.
    ENDLOOP.
"READ IT_CV FOR ACCOUNT FOR RATE_CARD
    READ TABLE IT_CV INTO CV_TIME WITH TABLE KEY DIM_UPPER_CASE = 'ACCOUNT'.
    LOOP AT CV_ACCOUNT-MEMBER INTO WA_MEMBER.
      IF SY-TABIX = 1 AND WA_MEMBER = 'RP000430' .
        FACTOR2 = 1.
        ELSE.
        FACTOR2 = 0.
      ENDIF.
    ENDLOOP.

"READ IT_CV FOR EMPLOYEE FOR RATE_CARD
***    READ TABLE IT_CV INTO CV_TIME WITH TABLE KEY DIM_UPPER_CASE = 'EMPLOYEE'.
***    LOOP AT CV_EMPLOYEE-MEMBER INTO WA_MEMBER.
***  "CHECK IF EMPLOYEE IS UNDER TOTAL_ITSSROLES
***    READ TABLE LT_ITSSROLES TRANSPORTING NO FIELDS
***    WITH KEY TABLE_LINE = WA_MEMBER.
***    IF SY-SUBRC = 0.
***        FACTOR2 = 1.
***
***        ELSE.
***        FACTOR2 = 0.
***        ENDIF.
***    ENDLOOP.

    LOOP AT LT_CV_EMPLOYEE INTO WA_MEMBER.
  "CHECK IF EMPLOYEE IS UNDER TOTAL_ITSSROLES
    READ TABLE LT_ITSSROLES TRANSPORTING NO FIELDS
    WITH KEY TABLE_LINE = WA_MEMBER.
    IF SY-SUBRC = 0.
        FACTOR2 = 1.

        ELSE.
        FACTOR2 = 0.
        ENDIF.
    ENDLOOP.

ENDIF. "SUBD_ID = ITS_R
********************* DATA VARIABLES *************************
    DATA: LT_FINAL      TYPE REF TO DATA,
          LS_REC        TYPE REF TO DATA,
          LS_RESULT_REC TYPE REF TO DATA.

    FIELD-SYMBOLS: <LT_FINAL>      TYPE STANDARD TABLE,
                   <LS_REC>        TYPE ANY,
                   <LS_RESULT_REC> TYPE ANY.

    FIELD-SYMBOLS: <LS_RESOURCE_ACCT> TYPE ANY,
                   <LS_AUDITTRAIL>    TYPE ANY,
                   <LS_CATEGORY>      TYPE ANY,
                   <LS_ENTITY>        TYPE ANY,
                   <LS_EMPLOYEE>      TYPE ANY,
                   <LS_RPTCURRENCY>   TYPE ANY,
                   <LS_TIME>          TYPE ANY,
                   <LS_SIGNEDDATA>    TYPE ANY.

**********************************************************************
************************ASSIGN STRUCTURE OF INCOMING MODEL ***********
    DATA:  LT_DIM_LIST    TYPE UJA_T_DIM_LIST,
           LO_APPL_MGR    TYPE REF TO IF_UJA_APPLICATION_MANAGER,
           LO_QUERY       TYPE REF TO IF_UJO_QUERY,
           LR_DATA        TYPE REF TO DATA,
           LS_APPLICATION TYPE UJA_S_APPLICATION,
           LS_DIMENSIONS  TYPE UJA_S_DIMENSION,
           LT_MESSAGE     TYPE UJ0_T_MESSAGE.
    FIELD-SYMBOLS: <CT_DATA>   TYPE  STANDARD TABLE,
                   <DATA_LINE> TYPE ANY.

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

    ASSIGN LR_DATA->* TO <CT_DATA>.

************ ASSIGN WAs **********************************************
"CT_DATA WILL BE X * 10
"<CT_DATA> WILL BE X * 9
    CREATE DATA LT_FINAL LIKE CT_DATA.
    ASSIGN LT_FINAL->* TO <LT_FINAL>.
    CREATE DATA LS_RESULT_REC LIKE LINE OF CT_DATA.
    ASSIGN LS_RESULT_REC->* TO <LS_RESULT_REC>.
    CREATE DATA LS_REC LIKE LINE OF CT_DATA.
    ASSIGN LS_REC->* TO <LS_REC>.
***********************************************************************

"CHECK IF DATA IS COMING FROM ITS_RATE_CARD


***************CHECK AND POPULATE ACCOUNT DRIVER AND TARGET ACCOUNT****
IF SUB_ID = 'ITS_R' OR SUB_ID = 'DRVRS' .
    IF LT_TARGET_ACCT IS INITIAL.
      "DO NOTHING
      ELSEIF LT_TARGET_ACCT IS NOT INITIAL AND FACTOR1 = 1.
        "READ RESOURCE MODEL TO GET THE ENTITY AND EMPLOYEE AFFECTED
            CLEAR: LT_SEL, LS_SEL.

            LS_SEL-DIMENSION = 'AUDITTRAIL'.
            LS_SEL-ATTRIBUTE = 'ID'.
            LS_SEL-SIGN = 'I'.
            LS_SEL-OPTION = 'EQ'.
            LS_SEL-LOW = 'INPUT'.
            APPEND LS_SEL TO LT_SEL.

            LS_SEL-DIMENSION = 'RPTCURRENCY'.
            LS_SEL-ATTRIBUTE = 'ID'.
            LS_SEL-SIGN = 'I'.
            LS_SEL-OPTION = 'EQ'.
            LS_SEL-LOW = 'LC'.
            APPEND LS_SEL TO LT_SEL.

            LS_SEL-DIMENSION = 'CATEGORY'.
            LS_SEL-ATTRIBUTE = 'ID'.
            LS_SEL-SIGN = 'I'.
            LS_SEL-OPTION = 'EQ'.
            LS_SEL-LOW = WORKING_CATEGORY.
            APPEND LS_SEL TO LT_SEL.

            LOOP AT LT_TARGET_ACCT INTO WA_MEMBER.
              LS_SEL-DIMENSION = 'RESOURCE_ACCT'.
              LS_SEL-ATTRIBUTE = 'ID'.
              LS_SEL-SIGN = 'I'.
              LS_SEL-OPTION = 'EQ'.
              LS_SEL-LOW = WA_MEMBER.
              APPEND LS_SEL TO LT_SEL.
            ENDLOOP.

            IF FACTOR2 = 1 AND SUB_ID = 'ITS_R'.
              "THESE WILL BE FOR ITS_RATE_CARD HENCE INCLUDE EMPLOYEES THAT ARE IN LT_ITSSROLES
            LOOP AT LT_CV_EMPLOYEE INTO WA_MEMBER.
              LS_SEL-DIMENSION = 'EMPLOYEE'.
              LS_SEL-ATTRIBUTE = 'ID'.
              LS_SEL-SIGN = 'I'.
              LS_SEL-OPTION = 'EQ'.
              LS_SEL-LOW = WA_MEMBER.
              APPEND LS_SEL TO LT_SEL.
            ENDLOOP.

            LOOP AT LT_TIMESCOPE INTO LS_TIME.
              LS_SEL-DIMENSION = 'TIME'.
              LS_SEL-ATTRIBUTE = 'ID'.
              LS_SEL-SIGN = 'I'.
              LS_SEL-OPTION = 'EQ'.
              LS_SEL-LOW = LS_TIME-ID .
              APPEND LS_SEL TO LT_SEL.
            ENDLOOP.

            LOOP AT LT_ENTITY_DRV INTO WA_MEMBER.
              LS_SEL-DIMENSION = 'ENTITY'.
              LS_SEL-ATTRIBUTE = 'ID'.
              LS_SEL-SIGN = 'I'.
              LS_SEL-OPTION = 'EQ'.
              LS_SEL-LOW = WA_MEMBER.
              APPEND LS_SEL TO LT_SEL.
            ENDLOOP.

            ELSE.
            LS_SEL-DIMENSION = 'RESOURCE_ACCT'.
            LS_SEL-ATTRIBUTE = 'ID'.
            LS_SEL-SIGN = 'I'.
            LS_SEL-OPTION = 'EQ'.
            LS_SEL-LOW = 'RP000027'.
            APPEND LS_SEL TO LT_SEL.

            LS_SEL-DIMENSION = 'TIME'.
            LS_SEL-ATTRIBUTE = 'ID'.
            LS_SEL-SIGN = 'I'.
            LS_SEL-OPTION = 'EQ'.
            LS_SEL-LOW = 'XXXX.INP'.
            APPEND LS_SEL TO LT_SEL.

            LS_SEL-DIMENSION = 'TIME'.
            LS_SEL-ATTRIBUTE = 'ID'.
            LS_SEL-SIGN = 'I'.
            LS_SEL-OPTION = 'EQ'.
            LS_SEL-LOW = YEAR_INP.
            APPEND LS_SEL TO LT_SEL.

            ENDIF.

****            LOOP AT LT_TIMESCOPE INTO LS_TIME.
****              LS_SEL-DIMENSION = 'TIME'.
****              LS_SEL-ATTRIBUTE = 'ID'.
****              LS_SEL-SIGN = 'I'.
****              LS_SEL-OPTION = 'EQ'.
****              LS_SEL-LOW = LS_TIME-ID .
****              APPEND LS_SEL TO LT_SEL.
****            ENDLOOP.

****            LOOP AT LT_ENTITY_DRV INTO WA_MEMBER.
****              LS_SEL-DIMENSION = 'ENTITY'.
****              LS_SEL-ATTRIBUTE = 'ID'.
****              LS_SEL-SIGN = 'I'.
****              LS_SEL-OPTION = 'EQ'.
****              LS_SEL-LOW = WA_MEMBER.
****              APPEND LS_SEL TO LT_SEL.
****            ENDLOOP.

          LO_TOOLS->READ_MODEL_DATA(
            EXPORTING
              IT_SEL      = LT_SEL
            IMPORTING
              OUTPUT_DATA = <CT_DATA> ).

          CLEAR LT_RESOURCE_DRV.
          MOVE-CORRESPONDING <CT_DATA> TO LT_RESOURCE_DRV.
          CLEAR <CT_DATA>."TO SAVE MEMORY
          SORT LT_RESOURCE_DRV BY
          AUDITTRAIL CATEGORY EMPLOYEE ENTITY RESOURCE_ACCT RPTCURRENCY TIME.

        ENDIF.

        CLEAR: LT_TARGET_EMPLEE, LT_TARGET_ENTITY .
        IF LT_RESOURCE_DRV IS NOT INITIAL.
        LOOP AT LT_RESOURCE_DRV INTO LS_RESOURCE.
          APPEND LS_RESOURCE-EMPLOYEE TO LT_TARGET_EMPLEE .
          APPEND LS_RESOURCE-ENTITY TO LT_TARGET_ENTITY .
          ENDLOOP.

          SORT LT_TARGET_EMPLEE .
          DELETE ADJACENT DUPLICATES FROM LT_TARGET_EMPLEE .
          SORT LT_TARGET_ENTITY .
          DELETE ADJACENT DUPLICATES FROM LT_TARGET_ENTITY .
          ENDIF.

ENDIF.

*******************************************************************************************************

*************READ RESOURCE CUBE FOR THE TIMESCOPE BASED ON CATEGORY YEAR AND CATEGORY**********
"TX DATA TABLE
CLEAR: LT_SEL, LS_SEL.
    LS_SEL-DIMENSION = 'AUDITTRAIL'.
    LS_SEL-ATTRIBUTE = 'ID'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = 'INPUT'.
    APPEND LS_SEL TO LT_SEL.

    "CLEAR: LT_SEL, LS_SEL.
    LS_SEL-DIMENSION = 'RPTCURRENCY'.
    LS_SEL-ATTRIBUTE = 'ID'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = 'LC'.
    APPEND LS_SEL TO LT_SEL.

    LS_SEL-DIMENSION = 'CATEGORY'.
    LS_SEL-ATTRIBUTE = 'ID'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = WORKING_CATEGORY. "'BUDGET'.
    APPEND LS_SEL TO LT_SEL.

    LOOP AT LT_TIMESCOPE INTO LS_TIME.
      LS_SEL-DIMENSION = 'TIME'.
      LS_SEL-ATTRIBUTE = 'ID'.
      LS_SEL-SIGN = 'I'.
      LS_SEL-OPTION = 'EQ'.
      LS_SEL-LOW = LS_TIME-ID .
      APPEND LS_SEL TO LT_SEL.
    ENDLOOP.

    LOOP AT LT_CV_ENTITY INTO WA_MEMBER.
      LS_SEL-DIMENSION = 'ENTITY'.
      LS_SEL-ATTRIBUTE = 'ID'.
      LS_SEL-SIGN = 'I'.
      LS_SEL-OPTION = 'EQ'.
      LS_SEL-LOW = WA_MEMBER.
      APPEND LS_SEL TO LT_SEL.
    ENDLOOP.

    IF GATE5 = 1 AND SUB_ID ='NONE'."CHANGE3

     ELSE. "CHANGE3
      LOOP AT LT_CV_EMPLOYEE INTO WA_MEMBER.
        LS_SEL-DIMENSION = 'EMPLOYEE'.
        LS_SEL-ATTRIBUTE = 'ID'.
        LS_SEL-SIGN = 'I'.
        LS_SEL-OPTION = 'EQ'.
        LS_SEL-LOW = WA_MEMBER.
        APPEND LS_SEL TO LT_SEL.
      ENDLOOP.

      LS_SEL-DIMENSION = 'EMPLOYEE'.
      LS_SEL-ATTRIBUTE = 'ID'.
      LS_SEL-SIGN = 'I'.
      LS_SEL-OPTION = 'EQ'.
      LS_SEL-LOW = 'NO_EMPLOYEE'.
      APPEND LS_SEL TO LT_SEL.
      ENDIF.

      IF GATE5 = 1 AND SUB_ID ='NONE'."CHANGE3
        IF LT_PROFCTR_CHILD IS NOT INITIAL.
          LOOP AT LT_PROFCTR_CHILD INTO WA_MEMBER.
            LS_SEL-DIMENSION  = 'ENTITY'.
            LS_SEL-ATTRIBUTE  = 'ID'.
            LS_SEL-SIGN       = 'I'.
            LS_SEL-OPTION     = 'EQ'.
            LS_SEL-LOW        = WA_MEMBER.
            APPEND LS_SEL TO LT_SEL.
          ENDLOOP.
          ENDIF.

        ELSE. "CHANGE3
      IF LT_TARGET_ENTITY IS NOT INITIAL.
        LOOP AT LT_TARGET_ENTITY INTO WA_MEMBER.
          LS_SEL-DIMENSION  = 'ENTITY'.
          LS_SEL-ATTRIBUTE  = 'ID'.
          LS_SEL-SIGN       = 'I'.
          LS_SEL-OPTION     = 'EQ'.
          LS_SEL-LOW        = WA_MEMBER.
          APPEND LS_SEL TO LT_SEL.
        ENDLOOP.
        ENDIF.

      IF LT_TARGET_EMPLEE IS NOT INITIAL.
        LOOP AT LT_TARGET_EMPLEE INTO WA_MEMBER.
          LS_SEL-DIMENSION  = 'EMPLOYEE'.
          LS_SEL-ATTRIBUTE  = 'ID'.
          LS_SEL-SIGN       = 'I'.
          LS_SEL-OPTION     = 'EQ'.
          LS_SEL-LOW        = WA_MEMBER.
          APPEND LS_SEL TO LT_SEL.
        ENDLOOP.
        ENDIF.
        ENDIF.

    LO_TOOLS->READ_MODEL_DATA(
        EXPORTING
        IT_SEL      = LT_SEL
      IMPORTING
        OUTPUT_DATA = <CT_DATA> ).

    CLEAR LT_RESOURCE_TX.
    MOVE-CORRESPONDING <CT_DATA> TO LT_RESOURCE_TX.
    CLEAR <CT_DATA>."TO SAVE MEMORY
************************************************************************************

*************READ RESOURCE CUBE FOR THE YYYY.INP XXXX.INP BASED ON CATEGORY YEAR AND CATEGORY**********
"RESOURCE TABLE FOR FLAGS
    CLEAR: LT_SEL, LS_SEL.

    LS_SEL-DIMENSION = 'RPTCURRENCY'.
    LS_SEL-ATTRIBUTE = 'ID'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = 'LC'.
    APPEND LS_SEL TO LT_SEL.

    LS_SEL-DIMENSION = 'CATEGORY'.
    LS_SEL-ATTRIBUTE = 'ID'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = WORKING_CATEGORY.
    APPEND LS_SEL TO LT_SEL.

    LS_SEL-DIMENSION = 'TIME'.
    LS_SEL-ATTRIBUTE = 'ID'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = 'XXXX.INP'.
    APPEND LS_SEL TO LT_SEL.

    LS_SEL-DIMENSION = 'TIME'.
    LS_SEL-ATTRIBUTE = 'ID'.
    LS_SEL-SIGN = 'I'.
    LS_SEL-OPTION = 'EQ'.
    LS_SEL-LOW = YEAR_INP .
    APPEND LS_SEL TO LT_SEL.

    LOOP AT LT_CV_EMPLOYEE INTO WA_MEMBER.
      LS_SEL-DIMENSION = 'EMPLOYEE'.
      LS_SEL-ATTRIBUTE = 'ID'.
      LS_SEL-SIGN = 'I'.
      LS_SEL-OPTION = 'EQ'.
      LS_SEL-LOW = WA_MEMBER.
      APPEND LS_SEL TO LT_SEL.
    ENDLOOP.

    LOOP AT LT_TOTAL_ITS INTO WA_MEMBER.
      LS_SEL-DIMENSION = 'EMPLOYEE'.
      LS_SEL-ATTRIBUTE = 'ID'.
      LS_SEL-SIGN = 'I'.
      LS_SEL-OPTION = 'EQ'.
      LS_SEL-LOW = WA_MEMBER.
      APPEND LS_SEL TO LT_SEL.
    ENDLOOP.

    LOOP AT LT_TOTAL_ROLES INTO WA_MEMBER.
      LS_SEL-DIMENSION = 'EMPLOYEE'.
      LS_SEL-ATTRIBUTE = 'ID'.
      LS_SEL-SIGN = 'I'.
      LS_SEL-OPTION = 'EQ'.
      LS_SEL-LOW = WA_MEMBER.
      APPEND LS_SEL TO LT_SEL.
    ENDLOOP.

    LOOP AT LT_ITSSROLES INTO WA_MEMBER.
      LS_SEL-DIMENSION = 'EMPLOYEE'.
      LS_SEL-ATTRIBUTE = 'ID'.
      LS_SEL-SIGN = 'I'.
      LS_SEL-OPTION = 'EQ'.
      LS_SEL-LOW = WA_MEMBER.
      APPEND LS_SEL TO LT_SEL.
    ENDLOOP.

      LS_SEL-DIMENSION = 'EMPLOYEE'.
      LS_SEL-ATTRIBUTE = 'ID'.
      LS_SEL-SIGN = 'I'.
      LS_SEL-OPTION = 'BT'.
      LS_SEL-LOW = 'EMP_S001' .
      LS_SEL-HIGH = 'EMP_S999' .
      APPEND LS_SEL TO LT_SEL.
      CLEAR LS_SEL-HIGH.

      LS_SEL-DIMENSION = 'EMPLOYEE'.
      LS_SEL-ATTRIBUTE = 'ID'.
      LS_SEL-SIGN = 'I'.
      LS_SEL-OPTION = 'EQ'.
      LS_SEL-LOW = 'NO_EMPLOYEE'.
      APPEND LS_SEL TO LT_SEL.

    LOOP AT LT_CV_ENTITY INTO WA_MEMBER.
      LS_SEL-DIMENSION = 'ENTITY'.
      LS_SEL-ATTRIBUTE = 'ID'.
      LS_SEL-SIGN = 'I'.
      LS_SEL-OPTION = 'EQ'.
      LS_SEL-LOW = WA_MEMBER.
      APPEND LS_SEL TO LT_SEL.
    ENDLOOP.

    LOOP AT LT_ENTITY INTO LS_ENTITY.
      LS_SEL-DIMENSION = 'ENTITY'.
      LS_SEL-ATTRIBUTE = 'ID'.
      LS_SEL-SIGN = 'I'.
      LS_SEL-OPTION = 'EQ'.
      LS_SEL-LOW = LS_ENTITY-PROFIT_CENTRE && 'BS'.
      APPEND LS_SEL TO LT_SEL.
    ENDLOOP.


      LS_SEL-DIMENSION = 'ENTITY'.
      LS_SEL-ATTRIBUTE = 'ID'.
      LS_SEL-SIGN = 'I'.
      LS_SEL-OPTION = 'EQ'.
      LS_SEL-LOW = 'NO_PROFITCENTREUK'.
      APPEND LS_SEL TO LT_SEL.

      LS_SEL-DIMENSION = 'ENTITY'.
      LS_SEL-ATTRIBUTE = 'ID'.
      LS_SEL-SIGN = 'I'.
      LS_SEL-OPTION = 'EQ'.
      LS_SEL-LOW = 'NO_PROFITCENTREME'.
      APPEND LS_SEL TO LT_SEL.

      LS_SEL-DIMENSION = 'ENTITY'.
      LS_SEL-ATTRIBUTE = 'ID'.
      LS_SEL-SIGN = 'I'.
      LS_SEL-OPTION = 'EQ'.
      LS_SEL-LOW = 'NO_PROFITCENTREEUR'.
      APPEND LS_SEL TO LT_SEL.

      LS_SEL-DIMENSION = 'ENTITY'.
      LS_SEL-ATTRIBUTE = 'ID'.
      LS_SEL-SIGN = 'I'.
      LS_SEL-OPTION = 'EQ'.
      LS_SEL-LOW = 'NO_PROFITCENTREASPAC'.
      APPEND LS_SEL TO LT_SEL.

      IF GATE5 = 1 AND SUB_ID ='NONE'."CHANGE3
        "GET EMPLOYEES FROM FLG TABLE ABOVE
        LOOP AT LT_RESOURCE_TX INTO LS_RESOURCE.
          CLEAR WA_MEMBER2.
          WA_MEMBER2 = LS_RESOURCE-EMPLOYEE.
          APPEND WA_MEMBER2 TO LT_EMP_ASPACME.
          ENDLOOP.
          SORT LT_EMP_ASPACME.
          DELETE ADJACENT DUPLICATES FROM LT_EMP_ASPACME.

          IF LT_EMP_ASPACME IS NOT INITIAL.
            LOOP AT LT_EMP_ASPACME INTO WA_MEMBER.
              LS_SEL-DIMENSION  = 'EMPLOYEE'.
              LS_SEL-ATTRIBUTE  = 'ID'.
              LS_SEL-SIGN       = 'I'.
              LS_SEL-OPTION     = 'EQ'.
              LS_SEL-LOW        = WA_MEMBER.
              APPEND LS_SEL TO LT_SEL.
            ENDLOOP.
            ENDIF.




        IF LT_PROFCTR_CHILD IS NOT INITIAL.
          LOOP AT LT_PROFCTR_CHILD INTO WA_MEMBER.
            LS_SEL-DIMENSION  = 'ENTITY'.
            LS_SEL-ATTRIBUTE  = 'ID'.
            LS_SEL-SIGN       = 'I'.
            LS_SEL-OPTION     = 'EQ'.
            LS_SEL-LOW        = WA_MEMBER.
            APPEND LS_SEL TO LT_SEL.
          ENDLOOP.
          ENDIF.

        ELSE. "CHANGE3
          IF LT_TARGET_ENTITY IS NOT INITIAL.
            LOOP AT LT_TARGET_ENTITY INTO WA_MEMBER.
              LS_SEL-DIMENSION  = 'ENTITY'.
              LS_SEL-ATTRIBUTE  = 'ID'.
              LS_SEL-SIGN       = 'I'.
              LS_SEL-OPTION     = 'EQ'.
              LS_SEL-LOW        = WA_MEMBER.
              APPEND LS_SEL TO LT_SEL.
            ENDLOOP.
            ENDIF.

          IF LT_TARGET_EMPLEE IS NOT INITIAL.
            LOOP AT LT_TARGET_EMPLEE INTO WA_MEMBER.
              LS_SEL-DIMENSION  = 'EMPLOYEE'.
              LS_SEL-ATTRIBUTE  = 'ID'.
              LS_SEL-SIGN       = 'I'.
              LS_SEL-OPTION     = 'EQ'.
              LS_SEL-LOW        = WA_MEMBER.
              APPEND LS_SEL TO LT_SEL.
            ENDLOOP.
            ENDIF.

        ENDIF."CHANGE3

    LO_TOOLS->READ_MODEL_DATA(
      EXPORTING
        IT_SEL      = LT_SEL
      IMPORTING
        OUTPUT_DATA = <CT_DATA> ).

    CLEAR LT_RESOURCE_FLG.
    MOVE-CORRESPONDING <CT_DATA> TO LT_RESOURCE_FLG.
    CLEAR <CT_DATA>."TO SAVE MEMORY

    "CALCULATE LOWER_LIM AND UPPER_LIM START
    IF LOWER_LIM IS INITIAL.
      CLEAR LS_RESOURCE.
      READ TABLE LT_RESOURCE_FLG
      INTO LS_RESOURCE
      WITH KEY
      AUDITTRAIL    = 'INPUT'
      CATEGORY      = WORKING_CATEGORY
      EMPLOYEE      = 'NO_EMPLOYEE'
      ENTITY        = 'NO_PROFITCENTREUK'
      RESOURCE_ACCT  = 'RP000002'
      RPTCURRENCY    = 'LC'
      TIME           = YEAR_INP
      BINARY SEARCH.
      IF SY-SUBRC = 0.
        LOWER_LIM = LS_RESOURCE-SIGNEDDATA.
      ELSE.
        LOWER_LIM = 0.
      ENDIF.
    ENDIF.

    IF UPPER_LIM IS INITIAL.
      CLEAR LS_RESOURCE.
      READ TABLE LT_RESOURCE_FLG
      INTO LS_RESOURCE
      WITH KEY
      AUDITTRAIL    = 'INPUT'
      CATEGORY      = WORKING_CATEGORY
      EMPLOYEE      = 'NO_EMPLOYEE'
      ENTITY        = 'NO_PROFITCENTREUK'
      RESOURCE_ACCT  = 'RP000003'
      RPTCURRENCY    = 'LC'
      TIME           = YEAR_INP
      BINARY SEARCH.
      IF SY-SUBRC = 0.
        UPPER_LIM = LS_RESOURCE-SIGNEDDATA.
      ELSE.
        UPPER_LIM = 0.
      ENDIF.
    ENDIF.
    "CALCULATE LOWER_LIM AND UPPER_LIM END

    IF NI_PERC IS INITIAL.
      CLEAR LS_RESOURCE.
      READ TABLE LT_RESOURCE_FLG
      INTO LS_RESOURCE
      WITH KEY
      AUDITTRAIL    = 'INPUT'
      CATEGORY      = WORKING_CATEGORY
      EMPLOYEE      = 'NO_EMPLOYEE'
      ENTITY        = 'NO_PROFITCENTREUK'
      RESOURCE_ACCT  = 'RP000001'
      RPTCURRENCY    = 'LC'
      TIME           = YEAR_INP
      BINARY SEARCH.
      IF SY-SUBRC = 0.
        NI_PERC = LS_RESOURCE-SIGNEDDATA.
      ELSE.
        NI_PERC = 0.
      ENDIF.
    ENDIF.

    IF VAR_ITAX IS INITIAL.
      CLEAR LS_RESOURCE.
      READ TABLE LT_RESOURCE_FLG
      INTO LS_RESOURCE
      WITH KEY
      AUDITTRAIL    = 'INPUT'
      CATEGORY      = WORKING_CATEGORY
      EMPLOYEE      = 'NO_EMPLOYEE'
      ENTITY        = 'NO_PROFITCENTREUK'
      RESOURCE_ACCT  = 'RP000420'
      RPTCURRENCY    = 'LC'
      TIME           = YEAR_INP
      BINARY SEARCH.
      IF SY-SUBRC = 0.
        VAR_ITAX  = LS_RESOURCE-SIGNEDDATA.
      ELSE.
        VAR_ITAX  = 0.
      ENDIF.
    ENDIF.

CLEAR WA_MEMBER.
LOOP AT LT_EMPLOYEE INTO LS_EMPLOYEE.
  "CHECK IF EMPLOYEE IS PART OF ITSS_ROLES
  WA_MEMBER = LS_EMPLOYEE-ID.
  READ TABLE LT_ITSSROLES TRANSPORTING
  NO FIELDS
  WITH KEY TABLE_LINE = WA_MEMBER.
  IF SY-SUBRC = 0 .
    APPEND WA_MEMBER TO LT_ITSSROLESROW.
    ENDIF.
  ENDLOOP.

*Process the Data sent from CT_DATA
"CHANGE3 START
IF SUB_ID = 'NONE' AND GATE5 = 1.

    "BELOW IS USED WHAN THE ADMIN INPUT SCHEDULE IS USED
    "POPULATE THE LT_ACCT_DRIVER
    WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000100'.
    WA_ACCT_DRIVER-TARGET_ACCT =  'RP900006'. "'RP010001'.
    APPEND WA_ACCT_DRIVER TO LT_ACCT_DRIVER.
    WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000101'.
    WA_ACCT_DRIVER-TARGET_ACCT =  'RP900006'. "'RP010001'.
    APPEND WA_ACCT_DRIVER TO LT_ACCT_DRIVER.
    WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000102'.
    WA_ACCT_DRIVER-TARGET_ACCT =  'RP900006'. "'RP010001'.
    APPEND WA_ACCT_DRIVER TO LT_ACCT_DRIVER.
    WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000103'.
    WA_ACCT_DRIVER-TARGET_ACCT =  'RP900006'. "'RP010001'.
    APPEND WA_ACCT_DRIVER TO LT_ACCT_DRIVER.
    WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000104'.
    WA_ACCT_DRIVER-TARGET_ACCT =  'RP900006'. "'RP010001'.
    APPEND WA_ACCT_DRIVER TO LT_ACCT_DRIVER.
    WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000105'.
    WA_ACCT_DRIVER-TARGET_ACCT = 'RP900006'. "'RP010001'.
    APPEND WA_ACCT_DRIVER TO LT_ACCT_DRIVER.
    WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000106'.
    WA_ACCT_DRIVER-TARGET_ACCT =  'RP900006'. "'RP010001'.
    APPEND WA_ACCT_DRIVER TO LT_ACCT_DRIVER.
    WA_ACCT_DRIVER-DRIVER_ACCT =  'RP000107'.
    WA_ACCT_DRIVER-TARGET_ACCT =  'RP900006'. "'RP010001'.
    APPEND WA_ACCT_DRIVER TO LT_ACCT_DRIVER.

    CLEAR LT_TARGET_ACCT.
    LOOP AT CV_ACCOUNT-MEMBER INTO WA_MEMBER.
      APPEND WA_MEMBER TO LT_CV_ACCOUNT.
      "CHECK LT_CV_ACCOUNT IF IT CONTAINS ACCOUNT DRIVERS !!!
      READ TABLE LT_ACCT_DRIVER
      INTO WA_ACCT_DRIVER
      WITH KEY
      DRIVER_ACCT =  WA_MEMBER
      BINARY SEARCH.
      IF SY-SUBRC = 0.
        APPEND WA_ACCT_DRIVER-TARGET_ACCT TO LT_TARGET_ACCT.

        ENDIF.
    ENDLOOP.





  CLEAR: GATE6, GATE7 .
  "GET ASPAC401 AND OR ASPAC901
    LOOP AT CT_DATA ASSIGNING <LS_REC>.
      CLEAR: LS_RESOURCE.
      <LS_RESULT_REC> = <LS_REC>.
      ASSIGN COMPONENT 'AUDITTRAIL'     OF STRUCTURE <LS_RESULT_REC> TO <LS_AUDITTRAIL>.
      ASSIGN COMPONENT 'CATEGORY'       OF STRUCTURE <LS_RESULT_REC> TO <LS_CATEGORY>.
      ASSIGN COMPONENT 'EMPLOYEE'       OF STRUCTURE <LS_RESULT_REC> TO <LS_EMPLOYEE>.
      ASSIGN COMPONENT 'ENTITY'         OF STRUCTURE <LS_RESULT_REC> TO <LS_ENTITY>.
      ASSIGN COMPONENT 'TIME'           OF STRUCTURE <LS_RESULT_REC> TO <LS_TIME>.
      ASSIGN COMPONENT 'RPTCURRENCY'    OF STRUCTURE <LS_RESULT_REC> TO <LS_RPTCURRENCY>.
      ASSIGN COMPONENT 'RESOURCE_ACCT'  OF STRUCTURE <LS_RESULT_REC> TO <LS_RESOURCE_ACCT>.
      ASSIGN COMPONENT 'SIGNEDDATA'     OF STRUCTURE <LS_RESULT_REC> TO <LS_SIGNEDDATA>.

      IF <LS_RESOURCE_ACCT> = 'RP000401' .
        ASPAC401 = <LS_SIGNEDDATA>.
        GATE6 = 1.
        ELSEIF <LS_RESOURCE_ACCT> = 'RP900001' .
          ASPAC901 = <LS_SIGNEDDATA>.
          GATE7 = 1.
            ENDIF.
      ENDLOOP.
  UNASSIGN <LS_REC>.
  CLEAR CT_DATA."SINCE THIS IS SET IS COMING FROM ASPAC OR ME CONTRACT DRIVERS
  CLEAR LT_RESOURCE_FLG2.

  "REPLACE 401 AND 901 WITH NEW ONES; ONLY FOR ASPAC
  LOOP AT LT_RESOURCE_FLG ASSIGNING <LS_RESOURCE_FLG>.
    IF <LS_RESOURCE_FLG>-RESOURCE_ACCT = 'RP000401' AND GATE6 = 1.
      READ TABLE LT_PROFCTR_CHILD TRANSPORTING NO FIELDS
      WITH KEY TABLE_LINE = <LS_RESOURCE_FLG>-ENTITY.
      IF SY-SUBRC = 0.
        <LS_RESOURCE_FLG>-SIGNEDDATA = ASPAC401 .
        APPEND <LS_RESOURCE_FLG> TO LT_RESOURCE_FLG2.
        ENDIF.
      ELSEIF <LS_RESOURCE_FLG>-RESOURCE_ACCT = 'RP900001' AND GATE7 = 1.
        READ TABLE LT_PROFCTR_CHILD TRANSPORTING NO FIELDS
        WITH KEY TABLE_LINE = <LS_RESOURCE_FLG>-ENTITY.
        IF SY-SUBRC = 0.
          <LS_RESOURCE_FLG>-SIGNEDDATA = ASPAC901 .
          APPEND <LS_RESOURCE_FLG> TO LT_RESOURCE_FLG2.
          ENDIF.
      ENDIF.
    ENDLOOP.


  "GET CT_DATA FROM LT_RESOURCE_TX.
  MOVE-CORRESPONDING LT_RESOURCE_TX TO CT_DATA.

  ENDIF.
  "CHANGE3 END

"WE ONLY WANT CALC FOR UNIQUE COMBO OF EMPLOYEE AND ENTITY AND LC ONLY
CLEAR LT_RESOURCE_DATA.
    LOOP AT CT_DATA ASSIGNING <LS_REC>.
      CLEAR: LS_RESOURCE.
      <LS_RESULT_REC> = <LS_REC>.
      ASSIGN COMPONENT 'AUDITTRAIL'     OF STRUCTURE <LS_RESULT_REC> TO <LS_AUDITTRAIL>.
      ASSIGN COMPONENT 'CATEGORY'       OF STRUCTURE <LS_RESULT_REC> TO <LS_CATEGORY>.
      ASSIGN COMPONENT 'EMPLOYEE'       OF STRUCTURE <LS_RESULT_REC> TO <LS_EMPLOYEE>.
      ASSIGN COMPONENT 'ENTITY'         OF STRUCTURE <LS_RESULT_REC> TO <LS_ENTITY>.
      ASSIGN COMPONENT 'TIME'           OF STRUCTURE <LS_RESULT_REC> TO <LS_TIME>.
      ASSIGN COMPONENT 'RPTCURRENCY'    OF STRUCTURE <LS_RESULT_REC> TO <LS_RPTCURRENCY>.
      ASSIGN COMPONENT 'RESOURCE_ACCT'  OF STRUCTURE <LS_RESULT_REC> TO <LS_RESOURCE_ACCT>.
      ASSIGN COMPONENT 'SIGNEDDATA'     OF STRUCTURE <LS_RESULT_REC> TO <LS_SIGNEDDATA>.

      LS_RESOURCE-AUDITTRAIL    = <LS_AUDITTRAIL>.
      LS_RESOURCE-CATEGORY      = <LS_CATEGORY>.
      LS_RESOURCE-EMPLOYEE      = <LS_EMPLOYEE>.
      LS_RESOURCE-ENTITY        = <LS_ENTITY>.
      LS_RESOURCE-TIME          = <LS_TIME>.
      LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
      LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
      LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
      IF <LS_RPTCURRENCY> <> 'LC'.
      CONTINUE.
      ENDIF.

      IF <LS_EMPLOYEE> = 'NO_EMPLOYEE'.
        CONTINUE.
        ENDIF.

      IF LS_RESOURCE-ENTITY(9) = 'NO_PROFIT'.
        CONTINUE.
        ENDIF.

      CLEAR LS_ENTITY.
      READ  TABLE LT_ENTITY
      INTO LS_ENTITY
      WITH KEY
      ID = LS_RESOURCE-ENTITY
      BINARY SEARCH.
      IF SY-SUBRC = 0.
        ECCSTATUS_ID = LS_ENTITY-ECCSTATUS_ID.
      ENDIF.

      IF ECCSTATUS_ID = 2 OR ECCSTATUS_ID = '02' OR ECCSTATUS_ID = 1 OR ECCSTATUS_ID = '01'.
        "OK
        ELSE.
          CONTINUE. "PROCESS ONLY EMPLOYEES THAT ARE ACTIVE
          ENDIF.

      APPEND LS_RESOURCE TO LT_RESOURCE_DATA.

      ENDLOOP.
  IF SUB_ID = 'ITS_R' OR SUB_ID = 'DRVRS' .
  "ADD DATA FROM LT_RESOURCE_DRV.

    IF FACTOR1 = 1.

      LOOP AT LT_RESOURCE_DRV INTO LS_RESOURCE.

        IF LS_RESOURCE-EMPLOYEE = 'NO_EMPLOYEE'.
          CONTINUE.
          ENDIF.

        IF LS_RESOURCE-ENTITY(9) = 'NO_PROFIT'.
          CONTINUE.
          ENDIF.

      CLEAR LS_ENTITY.
      READ  TABLE LT_ENTITY_DRV
      INTO LS_ENTITY
      WITH KEY
      ID = LS_RESOURCE-ENTITY
      BINARY SEARCH.
      IF SY-SUBRC = 0.
        ECCSTATUS_ID = LS_ENTITY-ECCSTATUS_ID.
        ELSE.
          CLEAR ECCSTATUS_ID .
          CLEAR LS_ENTITY.
          ENDIF.

      IF ECCSTATUS_ID = 2 OR ECCSTATUS_ID = '02' OR ECCSTATUS_ID = 1 OR ECCSTATUS_ID = '01'.
        "OK
        "use LS_RESOURCE-ENTITY TO MOVE LT_ENTITY ENTRY INTO LT_ENTITY
        APPEND LS_ENTITY TO LT_ENTITY.
        ELSE.
          CONTINUE. "PROCESS ONLY ENTITY THAT HAVE STATUS = 2
          ENDIF.

        APPEND LS_RESOURCE TO LT_RESOURCE_DATA.

        ENDLOOP.
        CLEAR LT_RESOURCE_DRV . "TO SAVE MEMORY

        SORT LT_ENTITY BY ID.
        DELETE ADJACENT DUPLICATES FROM LT_ENTITY COMPARING ID.

        ENDIF. "FACTOR1 = 1.
        ENDIF. "FACTOR1 = 1.

      SORT LT_RESOURCE_DATA BY ENTITY EMPLOYEE.
      DELETE ADJACENT DUPLICATES FROM LT_RESOURCE_DATA COMPARING ENTITY EMPLOYEE .

      CLEAR CT_DATA.

      MOVE-CORRESPONDING LT_RESOURCE_DATA TO CT_DATA.

"NOW PROCESS THE NEW CT_DATA
    LOOP AT CT_DATA ASSIGNING <LS_REC>.
      CLEAR: FLAG1, FACTOR1, FACTOR3, ENTITY_DRIVER.
      <LS_RESULT_REC> = <LS_REC>.
      ASSIGN COMPONENT 'AUDITTRAIL'     OF STRUCTURE <LS_RESULT_REC> TO <LS_AUDITTRAIL>.
      ASSIGN COMPONENT 'CATEGORY'       OF STRUCTURE <LS_RESULT_REC> TO <LS_CATEGORY>.
      ASSIGN COMPONENT 'EMPLOYEE'       OF STRUCTURE <LS_RESULT_REC> TO <LS_EMPLOYEE>.
      ASSIGN COMPONENT 'ENTITY'         OF STRUCTURE <LS_RESULT_REC> TO <LS_ENTITY>.
      ASSIGN COMPONENT 'TIME'           OF STRUCTURE <LS_RESULT_REC> TO <LS_TIME>.
      ASSIGN COMPONENT 'RPTCURRENCY'    OF STRUCTURE <LS_RESULT_REC> TO <LS_RPTCURRENCY>.
      ASSIGN COMPONENT 'RESOURCE_ACCT'  OF STRUCTURE <LS_RESULT_REC> TO <LS_RESOURCE_ACCT>.
      ASSIGN COMPONENT 'SIGNEDDATA'     OF STRUCTURE <LS_RESULT_REC> TO <LS_SIGNEDDATA>.

      VAR_ORIGAMOUNT  =   <LS_SIGNEDDATA>.
      ORIG_AUDITTRAIL =   <LS_AUDITTRAIL>.
      ORIG_CATEGORY   =   <LS_CATEGORY>.
      ORIG_EMPLOYEE   =   <LS_EMPLOYEE>.
      ORIG_ENTITY     =   <LS_ENTITY>.
      ORIG_ACCOUNT    =   <LS_RESOURCE_ACCT>.
      ORIG_CURRENCY   =   <LS_RPTCURRENCY>.
      ORIG_TIME       =   <LS_TIME>.


      CLEAR LS_RESOURCE.
      READ TABLE LT_RESOURCE_FLG
      INTO LS_RESOURCE
      WITH KEY
      AUDITTRAIL     = ORIG_AUDITTRAIL
      CATEGORY       = ORIG_CATEGORY
      EMPLOYEE       = ORIG_EMPLOYEE
      ENTITY         = ORIG_ENTITY
      RESOURCE_ACCT  = 'RP000425'
      RPTCURRENCY    = 'LC'
      TIME           = 'XXXX.INP'
      BINARY SEARCH.
      IF SY-SUBRC = 0.
        VAR_RP000425 = LS_RESOURCE-SIGNEDDATA.
      ELSE.
        VAR_RP000425 = 0.
      ENDIF.

      "RESOURCE_TYPE FROM EMPLOYEE PROPERTY
      CLEAR LS_EMPLOYEE.
      READ TABLE LT_EMPLOYEE
      INTO LS_EMPLOYEE
      WITH KEY
      ID = ORIG_EMPLOYEE
      BINARY SEARCH.
      IF SY-SUBRC = 0.
        RESOURCE_TYPE = LS_EMPLOYEE-RESOURCE_TYPE.
        ITSS_CC =  LS_EMPLOYEE-ITSS_CC.
        ELSE.
        CLEAR: RESOURCE_TYPE, ITSS_CC.
        CLEAR: ET_MESSAGE, L_LOG.
        L_LOG = 'Missing ITSS_CC / RESOURCE_TYPE - ' && ORIG_EMPLOYEE.
        CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).

      ENDIF.

      "REGION FROM ENTITY
      CLEAR LS_ENTITY.
      READ  TABLE LT_ENTITY
      INTO LS_ENTITY
      WITH KEY
      ID = ORIG_ENTITY
      BINARY SEARCH.
      IF SY-SUBRC = 0.
        REGION = LS_ENTITY-REG_CURRENCY.
        COMPANY = LS_ENTITY-COMPANY_CODE.
        LOCCURR = LS_ENTITY-CURRENCY.
        PROFIT_CENTRE = LS_ENTITY-PROFIT_CENTRE && 'BS'.

        ELSE.
        L_LOG = 'Missing Properties (REG_CURRENCY, COMPANY_CODE , CURRENCY , PROFIT_CENTRE - for' && ORIG_ENTITY.
        CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).

      ENDIF.

      IF REGION IS INITIAL OR COMPANY IS INITIAL OR PROFIT_CENTRE IS INITIAL . "OR LOCCURR IS INITIAL.
        L_LOG = 'Missing Properties (REG_CURRENCY, COMPANY_CODE , CURRENCY , PROFIT_CENTRE - for' && ORIG_ENTITY.
        CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).
        ENDIF.

      CASE REGION.
        WHEN 'AUD'.
          REGION = 'ASPAC'.
          ENTITY_DRIVER = 'NO_PROFITCENTREASPAC'.
        WHEN 'AED'.
          REGION = 'ME'.
          ENTITY_DRIVER = 'NO_PROFITCENTREME'.
        WHEN 'EUR'.
          REGION = 'EU'.
          ENTITY_DRIVER = 'NO_PROFITCENTREEU'.
        WHEN 'GBP'.
          REGION = 'UK'.
          ENTITY_DRIVER = 'NO_PROFITCENTREUK'.
        WHEN OTHERS.
          REGION = ''.
      ENDCASE.

  "CHECK IF EMPLOYEE IS UNDER TOTAL_ITSSROLES
    READ TABLE LT_ITSSROLES TRANSPORTING NO FIELDS
    WITH KEY TABLE_LINE = ORIG_EMPLOYEE.
    IF SY-SUBRC = 0.
      ITSS_FLAG = 'Y'.
      ELSE.
        ITSS_FLAG = 'N'.
        ENDIF.

  "CHECK IF EMPLOYEE IS UNDER TOTAL_EMPLOYEES
    READ TABLE LT_TOTAL_EMPLOYEES TRANSPORTING NO FIELDS
    WITH KEY TABLE_LINE = ORIG_EMPLOYEE.
    IF SY-SUBRC = 0.
      EMPLOYEE_FLAG = 'Y'.
      ELSE.
        EMPLOYEE_FLAG = 'N'.
        ENDIF.

  "CHECK IF EMPLOYEE IS UNDER TOTAL_ROLES
    READ TABLE LT_TOTAL_ROLES TRANSPORTING NO FIELDS
    WITH KEY TABLE_LINE = ORIG_EMPLOYEE.
    IF SY-SUBRC = 0.
      ROLES_FLAG = 'Y'.
      ELSE.
        ROLES_FLAG = 'N'.
        ENDIF.

  "GET VAR_ITAX_ROW
    CLEAR LS_RESOURCE.
    READ TABLE LT_RESOURCE_FLG
    INTO LS_RESOURCE
    WITH KEY
    AUDITTRAIL      = 'INPUT'
    CATEGORY        = WORKING_CATEGORY
    EMPLOYEE        = 'NO_EMPLOYEE'
    ENTITY          = PROFIT_CENTRE
    RESOURCE_ACCT   = 'RP000420'
    RPTCURRENCY     = 'LC'
    TIME            = YEAR_INP
    BINARY SEARCH.
    IF SY-SUBRC = 0.
      VAR_ITAX_ROW  = LS_RESOURCE-SIGNEDDATA.
    ELSE.
      VAR_ITAX_ROW  = 0.
    ENDIF.

CASE SUB_ID.

  WHEN 'NONE'. "DEFAULT LOGIC

    IF FACTOR2 = 1 . "FOR ITS RATE CARD
      CONTINUE.
      ENDIF.

    "BLANK CLEARING OF TOTAL_STSTACCT AND TOTAL_MONTHLYVALUES AS THESE WILL BE RE CALCULATED FURTHER DOWN THE LINE.

    LOOP AT LT_TIMESCOPE INTO LS_TIME.

      <LS_SIGNEDDATA> = 0.
      <LS_TIME> = LS_TIME-ID .
      LOOP AT LT_TOTAL_STATACCT INTO WA_MEMBER.
      <LS_RESOURCE_ACCT> = WA_MEMBER.
      COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
      ENDLOOP.

      LOOP AT LT_TOTAL_MONTHLYVALUES INTO WA_MEMBER.
      <LS_RESOURCE_ACCT> = WA_MEMBER.
      COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
      ENDLOOP.

      ENDLOOP.
      <LS_SIGNEDDATA>     = VAR_ORIGAMOUNT.
      <LS_RESOURCE_ACCT>  = ORIG_ACCOUNT .
      <LS_TIME>           = ORIG_TIME .

    CLEAR: FACTOR1, FACTOR2, VAR_PENSIONALLOW .
     LOOP AT LT_PENSIONALLOW INTO WA_MEMBER.
       FACTOR1 = FACTOR2.
       CLEAR LS_RESOURCE.
       READ TABLE LT_RESOURCE_FLG
       INTO LS_RESOURCE
       WITH KEY
       AUDITTRAIL    = ORIG_AUDITTRAIL
       CATEGORY      = ORIG_CATEGORY
       EMPLOYEE      = ORIG_EMPLOYEE
       ENTITY        = ORIG_ENTITY
       RESOURCE_ACCT  = WA_MEMBER
       RPTCURRENCY    = ORIG_CURRENCY
       TIME           = YEAR_INP
       BINARY SEARCH.
       IF SY-SUBRC = 0 .
         FACTOR2 =  LS_RESOURCE-SIGNEDDATA + FACTOR1.
         VAR_PENSIONALLOW  =  FACTOR2.
         ENDIF.
      ENDLOOP.
*************************************************************************************************
****"PUT CALC HERE TO PROCESS THE ADMIN DRIVERS
****"OTH_UK 1 OTH_ME 1 OTH_ASPAC 1
****  IF ( ( RESOURCE_TYPE = 'E' AND EMPLOYEE_FLAG = 'Y' )  OR ( VAR_RP000425 = 0 AND ROLES_FLAG = 'Y' ) )
****    AND LT_TARGET_ACCT IS NOT INITIAL AND ( REGION = 'UK' OR REGION = 'ME' OR REGION = 'ASPAC').  "CALC IS ONLY FOR PERM EMP OR PERM ROLES
****
****    CLEAR: FACTOR1, FACTOR2,FACTOR3, FACTOR4,FACTOR5, FACTOR6,FACTOR7, FACTOR8.
****
****    CLEAR LS_RESOURCE.
****    READ TABLE LT_RESOURCE_FLG
****    INTO LS_RESOURCE
****    WITH KEY
****    AUDITTRAIL = ORIG_AUDITTRAIL
****    CATEGORY   = ORIG_CATEGORY
****    EMPLOYEE   = ORIG_EMPLOYEE
****    ENTITY     = ORIG_ENTITY
****    RESOURCE_ACCT  = 'RP000400'
****    RPTCURRENCY    = ORIG_CURRENCY
****    TIME           = YEAR_INP
****    BINARY SEARCH.
****    IF SY-SUBRC = 0.
****      FACTOR1 = LS_RESOURCE-SIGNEDDATA.
****    ELSE.
****      FACTOR1 = 0.
****    ENDIF.
****
****    READ TABLE LT_TARGET_ACCT INTO WA_MEMBER
****    WITH KEY TABLE_LINE = 'RP000401'
****    BINARY SEARCH.
****    IF SY-SUBRC = 0.
****      FACTOR2 = 1.
****    ELSE.
****      FACTOR2 = 0.
****    ENDIF.
****
****    READ TABLE LT_TARGET_ACCT INTO WA_MEMBER
****    WITH KEY TABLE_LINE = 'RP900001'
****    BINARY SEARCH.
****    IF SY-SUBRC = 0.
****      FACTOR3 = 1.
****    ELSE.
****      FACTOR3 = 0.
****    ENDIF.
****
****
****    IF FACTOR1 <> 1 AND FACTOR2 = 1 AND FACTOR3 = 1. "'RP000400'
****      CLEAR LS_RESOURCE.
****      READ TABLE LT_RESOURCE_FLG
****      INTO LS_RESOURCE
****      WITH KEY
****      AUDITTRAIL = ORIG_AUDITTRAIL
****      CATEGORY   = ORIG_CATEGORY
****      EMPLOYEE   = 'NO_EMPLOYEE'
****      ENTITY     = ENTITY_DRIVER "'NO_PROFITCENTREUK'
****      RESOURCE_ACCT  = 'RP000401'
****      RPTCURRENCY    = ORIG_CURRENCY
****      TIME           = YEAR_INP
****      BINARY SEARCH.
****      IF SY-SUBRC = 0.
****        <LS_SIGNEDDATA>     = LS_RESOURCE-SIGNEDDATA.
****        <LS_RESOURCE_ACCT>  = 'RP000401' .
****        <LS_TIME>           = YEAR_INP.
****        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
****        "BELOW UPDATING THE LT_RESOURCE_FLG START
****        CLEAR LS_RESOURCE.
****        LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
****        LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
****        LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
****        LS_RESOURCE-ENTITY      = <LS_ENTITY>.
****        LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
****        LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
****        LS_RESOURCE-TIME          = <LS_TIME>.
****        LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
****        LOOP AT LT_RESOURCE_FLG ASSIGNING <LS_RESOURCE> WHERE
****          AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
****          AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
****          AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
****          AND RPTCURRENCY = <LS_RPTCURRENCY> AND
****          TIME = <LS_TIME>.
****          <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
****          ENDLOOP.
****          IF SY-SUBRC <> 0.
****            INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_FLG.
****            ENDIF.
****        "ABOVE UPDATING THE LT_RESOURCE_FLG END
****      ENDIF.
****
****      CLEAR LS_RESOURCE.
****      READ TABLE LT_RESOURCE_FLG
****      INTO LS_RESOURCE
****      WITH KEY
****      AUDITTRAIL = ORIG_AUDITTRAIL
****      CATEGORY   = ORIG_CATEGORY
****      EMPLOYEE   = 'NO_EMPLOYEE'
****      ENTITY     = ENTITY_DRIVER "'NO_PROFITCENTREUK'
****      RESOURCE_ACCT  = 'RP900001'
****      RPTCURRENCY    = ORIG_CURRENCY
****      TIME           = YEAR_INP
****      BINARY SEARCH.
****      IF SY-SUBRC = 0.
****        <LS_SIGNEDDATA>     = LS_RESOURCE-SIGNEDDATA.
****        <LS_RESOURCE_ACCT>  = 'RP900001' .
****        <LS_TIME>           = YEAR_INP.
****        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
****        "BELOW UPDATING THE LT_RESOURCE_FLG START
****        CLEAR LS_RESOURCE.
****        LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
****        LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
****        LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
****        LS_RESOURCE-ENTITY      = <LS_ENTITY>.
****        LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
****        LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
****        LS_RESOURCE-TIME          = <LS_TIME>.
****        LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
****        LOOP AT LT_RESOURCE_FLG ASSIGNING <LS_RESOURCE> WHERE
****          AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
****          AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
****          AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
****          AND RPTCURRENCY = <LS_RPTCURRENCY> AND
****          TIME = <LS_TIME>.
****          <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
****          ENDLOOP.
****          IF SY-SUBRC <> 0.
****            INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_FLG.
****            ENDIF.
****        "ABOVE UPDATING THE LT_RESOURCE_FLG END
****      ENDIF.
****      ENDIF.
****    ENDIF.
****
****"OTH_UK 7 AND OTH_UK 8
****  IF ( ( RESOURCE_TYPE = 'E' AND EMPLOYEE_FLAG = 'Y' )  OR ( VAR_RP000425 = 0 AND ROLES_FLAG = 'Y' ) )
****    AND LT_TARGET_ACCT IS NOT INITIAL AND ( REGION = 'UK' ).  "CALC IS ONLY FOR PERM EMP OR PERM ROLES
****
****    CLEAR: FACTOR1, FACTOR2,FACTOR3, FACTOR4,FACTOR5, FACTOR6,FACTOR7, FACTOR8.
****
****    READ TABLE LT_TARGET_ACCT INTO WA_MEMBER
****    WITH KEY TABLE_LINE = 'RP900006'
****    BINARY SEARCH.
****    IF SY-SUBRC = 0.
****      READ TABLE LT_RESOURCE_FLG
****      INTO LS_RESOURCE
****      WITH KEY
****      AUDITTRAIL = ORIG_AUDITTRAIL
****      CATEGORY   = ORIG_CATEGORY
****      EMPLOYEE   = ORIG_EMPLOYEE
****      ENTITY     = ORIG_ENTITY
****      RESOURCE_ACCT  = 'RP900006'
****      RPTCURRENCY    = ORIG_CURRENCY
****      TIME           = 'XXXX.INP'
****      BINARY SEARCH.
****      IF SY-SUBRC = 0.
****        FACTOR1 = LS_RESOURCE-SIGNEDDATA.
****      ELSE.
****        FACTOR1 = 0.
****      ENDIF.
****
****      CASE FACTOR1.
****
****        WHEN 10 .
****          ACCOUNT_DRIVER = 'RP000010'.
****        WHEN 11 .
****          ACCOUNT_DRIVER = 'RP000011'.
****        WHEN 12 .
****          ACCOUNT_DRIVER = 'RP000012'.
****        WHEN 13 .
****          ACCOUNT_DRIVER = 'RP000013'.
****        WHEN 14 .
****          ACCOUNT_DRIVER = 'RP000014'.
****        WHEN OTHERS.
****          ACCOUNT_DRIVER = 'DONOTHING'.
****        ENDCASE.
****
****        CLEAR LS_RESOURCE.
****        READ TABLE LT_RESOURCE_FLG
****        INTO LS_RESOURCE
****        WITH KEY
****        AUDITTRAIL = ORIG_AUDITTRAIL
****        CATEGORY   = ORIG_CATEGORY
****        EMPLOYEE   = 'NO_EMPLOYEE'
****        ENTITY     = ENTITY_DRIVER
****        RESOURCE_ACCT  =  ACCOUNT_DRIVER
****        RPTCURRENCY    = ORIG_CURRENCY
****        TIME           = YEAR_INP
****        BINARY SEARCH.
****        IF SY-SUBRC = 0.
****          <LS_SIGNEDDATA>     = LS_RESOURCE-SIGNEDDATA.
****          <LS_RESOURCE_ACCT>  = 'RP010008' .
****          <LS_TIME>           = YEAR_INP.
****          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
****          "BELOW UPDATING THE LT_RESOURCE_FLG START
****          CLEAR LS_RESOURCE.
****          LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
****          LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
****          LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
****          LS_RESOURCE-ENTITY      = <LS_ENTITY>.
****          LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
****          LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
****          LS_RESOURCE-TIME          = <LS_TIME>.
****          LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
****          LOOP AT LT_RESOURCE_FLG ASSIGNING <LS_RESOURCE> WHERE
****            AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
****            AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
****            AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
****            AND RPTCURRENCY = <LS_RPTCURRENCY> AND
****            TIME = <LS_TIME>.
****            <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
****            ENDLOOP.
****            IF SY-SUBRC <> 0.
****              INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_FLG.
****              ENDIF.
****          "ABOVE UPDATING THE LT_RESOURCE_FLG END
****          ENDIF.
****    ENDIF.
****
****    READ TABLE LT_TARGET_ACCT INTO WA_MEMBER
****    WITH KEY TABLE_LINE = 'RP900005'
****    BINARY SEARCH.
****    IF SY-SUBRC = 0.
****      READ TABLE LT_RESOURCE_FLG
****      INTO LS_RESOURCE
****      WITH KEY
****      AUDITTRAIL = ORIG_AUDITTRAIL
****      CATEGORY   = ORIG_CATEGORY
****      EMPLOYEE   = ORIG_EMPLOYEE
****      ENTITY     = ORIG_ENTITY
****      RESOURCE_ACCT  = 'RP900005'
****      RPTCURRENCY    = ORIG_CURRENCY
****      TIME           = 'XXXX.INP'
****      BINARY SEARCH.
****      IF SY-SUBRC = 0.
****        FACTOR1 = LS_RESOURCE-SIGNEDDATA.
****      ELSE.
****        FACTOR1 = 0.
****
****      ENDIF.
****
****      READ TABLE LT_RESOURCE_FLG
****      INTO LS_RESOURCE
****      WITH KEY
****      AUDITTRAIL = ORIG_AUDITTRAIL
****      CATEGORY   = ORIG_CATEGORY
****      EMPLOYEE   = ORIG_EMPLOYEE
****      ENTITY     = ORIG_ENTITY
****      RESOURCE_ACCT  = 'RP000027'
****      RPTCURRENCY    = ORIG_CURRENCY
****      TIME           = 'XXXX.INP'
****      BINARY SEARCH.
****      IF SY-SUBRC = 0.
****        FACTOR2 = LS_RESOURCE-SIGNEDDATA.
****        IF FACTOR2 = 1 .
****          WA_MEMBER   = 'RP000020'.
****          WA_MEMBERX  = 'RP000021'.
****          ELSE.
****            WA_MEMBER   = 'RP000021'.
****            WA_MEMBERX  = 'RP000020'.
****            ENDIF.
****      ELSE.
****        FACTOR2 = 0.
****        WA_MEMBER = 'NOACCOUNT'.
****      ENDIF.
****
****      CASE FACTOR1.
****
****        WHEN 10 .
****          ACCOUNT_DRIVER = 'RP000015'.
****        WHEN 11 .
****          ACCOUNT_DRIVER = 'RP000016'.
****        WHEN 12 .
****          ACCOUNT_DRIVER = 'RP000017'.
****        WHEN 13 .
****          ACCOUNT_DRIVER = 'RP000018'.
****        WHEN 14 .
****          ACCOUNT_DRIVER = 'RP000019'.
****        WHEN OTHERS.
****          ACCOUNT_DRIVER = 'DONOTHING'.
****        ENDCASE.
****
****        CLEAR LS_RESOURCE.
****        READ TABLE LT_RESOURCE_FLG
****        INTO LS_RESOURCE
****        WITH KEY
****        AUDITTRAIL = ORIG_AUDITTRAIL
****        CATEGORY   = ORIG_CATEGORY
****        EMPLOYEE   = 'NO_EMPLOYEE'
****        ENTITY     = ENTITY_DRIVER
****        RESOURCE_ACCT  =  ACCOUNT_DRIVER
****        RPTCURRENCY    = ORIG_CURRENCY
****        TIME           = YEAR_INP
****        BINARY SEARCH.
****        IF SY-SUBRC = 0.
****          <LS_SIGNEDDATA>     = LS_RESOURCE-SIGNEDDATA.
****          <LS_RESOURCE_ACCT>  = WA_MEMBER . "'RP000020' .
****          <LS_TIME>           = YEAR_INP.
****          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
****          "BELOW UPDATING THE LT_RESOURCE_FLG START
****          CLEAR LS_RESOURCE.
****          LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
****          LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
****          LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
****          LS_RESOURCE-ENTITY      = <LS_ENTITY>.
****          LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
****          LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
****          LS_RESOURCE-TIME          = <LS_TIME>.
****          LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
****          LOOP AT LT_RESOURCE_FLG ASSIGNING <LS_RESOURCE> WHERE
****            AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
****            AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
****            AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
****            AND RPTCURRENCY = <LS_RPTCURRENCY> AND
****            TIME = <LS_TIME>.
****            <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
****            ENDLOOP.
****            IF SY-SUBRC <> 0.
****              INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_FLG.
****              ENDIF.
****          "ABOVE UPDATING THE LT_RESOURCE_FLG END
****
****          <LS_SIGNEDDATA>     = 0.
****          <LS_RESOURCE_ACCT>  = WA_MEMBERX .
****          <LS_TIME>           = YEAR_INP.
****          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
****          "BELOW UPDATING THE LT_RESOURCE_FLG START
****          CLEAR LS_RESOURCE.
****          LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
****          LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
****          LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
****          LS_RESOURCE-ENTITY      = <LS_ENTITY>.
****          LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
****          LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
****          LS_RESOURCE-TIME          = <LS_TIME>.
****          LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
****          LOOP AT LT_RESOURCE_FLG ASSIGNING <LS_RESOURCE> WHERE
****            AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
****            AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
****            AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
****            AND RPTCURRENCY = <LS_RPTCURRENCY> AND
****            TIME = <LS_TIME>.
****            <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
****            ENDLOOP.
****            IF SY-SUBRC <> 0.
****              INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_FLG.
****              ENDIF.
****          "ABOVE UPDATING THE LT_RESOURCE_FLG END
****
****          ENDIF.
****
****    ENDIF.
****    ENDIF. "OTH_UK 7 AND OTH_UK 8 END
****
****"OTH_ME 4
******  IF ( ( RESOURCE_TYPE = 'E' AND EMPLOYEE_FLAG = 'Y' )  OR ( VAR_RP000425 = 0 AND ROLES_FLAG = 'Y' ) )
******    AND LT_TARGET_ACCT IS NOT INITIAL AND ( REGION = 'ME' OR REGION = 'ASPAC' ).  "CALC IS ONLY FOR PERM EMP OR PERM ROLES
****  IF LT_TARGET_ACCT IS NOT INITIAL AND ( REGION = 'ME' ) AND GATE1 = 1 AND GATE2 = 1.  "CALC IS ONLY FOR PERM EMP OR PERM ROLES
****
****
****    CLEAR: FACTOR1, FACTOR2,FACTOR3, FACTOR4,FACTOR5, FACTOR6,FACTOR7, FACTOR8.
****
****    READ TABLE LT_TARGET_ACCT INTO WA_MEMBER
****    WITH KEY TABLE_LINE = 'RP900006'
****    BINARY SEARCH.
****    IF SY-SUBRC = 0.
****      READ TABLE LT_RESOURCE_FLG
****      INTO LS_RESOURCE
****      WITH KEY
****      AUDITTRAIL = ORIG_AUDITTRAIL
****      CATEGORY   = ORIG_CATEGORY
****      EMPLOYEE   = ORIG_EMPLOYEE
****      ENTITY     = ORIG_ENTITY
****      RESOURCE_ACCT  = 'RP900006'
****      RPTCURRENCY    = ORIG_CURRENCY
****      TIME           = 'XXXX.INP'
****      BINARY SEARCH.
****      IF SY-SUBRC = 0.
****        FACTOR1 = LS_RESOURCE-SIGNEDDATA.
****      ELSE.
****        FACTOR1 = 0.
****      ENDIF.
****
****      CASE FACTOR1.
****
****        WHEN 10 .
****          ACCOUNT_DRIVER = 'RP000100'.
****        WHEN 11 .
****          ACCOUNT_DRIVER = 'RP000101'.
****        WHEN 12 .
****          ACCOUNT_DRIVER = 'RP000102'.
****        WHEN 13 .
****          ACCOUNT_DRIVER = 'RP000103'.
****        WHEN 14 .
****          ACCOUNT_DRIVER = 'RP000104'.
****        WHEN 15 .
****          ACCOUNT_DRIVER = 'RP000105'.
****        WHEN 16 .
****          ACCOUNT_DRIVER = 'RP000106'.
****        WHEN 17 .
****          ACCOUNT_DRIVER = 'RP000107'.
****        WHEN OTHERS.
****          ACCOUNT_DRIVER = 'DONOTHING'.
****        ENDCASE.
****
****        CLEAR LS_RESOURCE.
****        READ TABLE LT_RESOURCE_FLG
****        INTO LS_RESOURCE
****        WITH KEY
****        AUDITTRAIL      = ORIG_AUDITTRAIL
****        CATEGORY        = ORIG_CATEGORY
****        EMPLOYEE        = 'NO_EMPLOYEE'
****        ENTITY          = ENTITY_DRIVER
****        RESOURCE_ACCT   =  ACCOUNT_DRIVER
****        RPTCURRENCY     = ORIG_CURRENCY
****        TIME            = YEAR_INP
****        BINARY SEARCH.
****        IF SY-SUBRC = 0.
****          <LS_SIGNEDDATA>     = LS_RESOURCE-SIGNEDDATA.
****          <LS_RESOURCE_ACCT>  = 'RP010008' .
****          <LS_TIME>           = YEAR_INP.
****          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
****          "BELOW UPDATING THE LT_RESOURCE_FLG START
****          CLEAR LS_RESOURCE.
****          LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
****          LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
****          LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
****          LS_RESOURCE-ENTITY      = <LS_ENTITY>.
****          LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
****          LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
****          LS_RESOURCE-TIME          = <LS_TIME>.
****          LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
****          LOOP AT LT_RESOURCE_FLG ASSIGNING <LS_RESOURCE> WHERE
****            AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
****            AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
****            AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
****            AND RPTCURRENCY = <LS_RPTCURRENCY> AND
****            TIME = <LS_TIME>.
****            <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
****            ENDLOOP.
****            IF SY-SUBRC <> 0.
****              INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_FLG.
****              ENDIF.
****          "ABOVE UPDATING THE LT_RESOURCE_FLG END
****          ENDIF.
****    ENDIF.
****
****    ENDIF.

*************************************************************************************************
"GEN CALC 4 - Calculate other salary increase monthly values --START
      "// Calculate other salary increase
      "If RP900003 = """", do nothing (THIS WILL GIVE YOU YYYYMM IN SIGNEDDATA)
      "IF RP900003 IS IN THE PAST OF CURR PERIOD CLEAR DATA
      "IF 402 OR 401 IS FACTORED IN BUT NO 900002 OR 900001 RESPECTIVELY JUST DO THE
  IF ( RESOURCE_TYPE = 'E' AND EMPLOYEE_FLAG = 'Y' )  OR ( VAR_RP000425 = 0 AND ROLES_FLAG = 'Y' ). "CALC IS ONLY FOR PERM EMP OR PERM ROLES

      CLEAR: FACTOR1, FACTOR2,FACTOR3, FACTOR4,FACTOR6, FACTOR6,TIME_STRING ,TIME_STRING2 ,TIME_STRING3,
             FACTOR7, FACTOR8.

      CLEAR LS_RESOURCE.
      READ TABLE LT_RESOURCE_FLG
      INTO LS_RESOURCE
      WITH KEY
      AUDITTRAIL     = ORIG_AUDITTRAIL
      CATEGORY       = ORIG_CATEGORY
      EMPLOYEE       = ORIG_EMPLOYEE
      ENTITY         = ORIG_ENTITY
      RESOURCE_ACCT  = 'RP900003' "THIS WILL GIVE YOU YYYYMM IN SIGNEDDATA
      RPTCURRENCY    = ORIG_CURRENCY
      TIME           = YEAR_INP
      BINARY SEARCH.
      IF SY-SUBRC = 0 AND LS_RESOURCE-SIGNEDDATA IS NOT INITIAL.
        TIME_STRING = LS_RESOURCE-SIGNEDDATA.
        TIME_STRING = TIME_STRING(6) && '00'.
        CONDENSE TIME_STRING NO-GAPS.

      ELSE.
        TIME_STRING = 0.
      ENDIF.

      CLEAR LS_RESOURCE.
      READ TABLE LT_RESOURCE_FLG
      INTO LS_RESOURCE
      WITH KEY
      AUDITTRAIL = ORIG_AUDITTRAIL
      CATEGORY   = ORIG_CATEGORY
      EMPLOYEE   = ORIG_EMPLOYEE
      ENTITY     = ORIG_ENTITY
      RESOURCE_ACCT  = 'RP010003' "FACTOR1
      RPTCURRENCY    = ORIG_CURRENCY
      TIME           = YEAR_INP
      BINARY SEARCH.
      IF SY-SUBRC = 0.
        FACTOR1 = LS_RESOURCE-SIGNEDDATA.
      ELSE.
        FACTOR1 = 0.
      ENDIF.

      CLEAR LS_RESOURCE.
      READ TABLE LT_RESOURCE_FLG
      INTO LS_RESOURCE
      WITH KEY
      AUDITTRAIL = ORIG_AUDITTRAIL
      CATEGORY   = ORIG_CATEGORY
      EMPLOYEE   = ORIG_EMPLOYEE
      ENTITY     = ORIG_ENTITY
      RESOURCE_ACCT  = 'RP000400' "FACTOR3
      RPTCURRENCY    = ORIG_CURRENCY
      TIME           = YEAR_INP
      BINARY SEARCH.
      IF SY-SUBRC = 0.
        FACTOR3 = LS_RESOURCE-SIGNEDDATA.
      ELSE.
        FACTOR3 = 0.
      ENDIF.

      CLEAR LS_RESOURCE.
      READ TABLE LT_RESOURCE_FLG
      INTO LS_RESOURCE
      WITH KEY
      AUDITTRAIL = ORIG_AUDITTRAIL
      CATEGORY   = ORIG_CATEGORY
      EMPLOYEE   = ORIG_EMPLOYEE
      ENTITY     = ORIG_ENTITY
      RESOURCE_ACCT  = 'RP000402' "FACTOR4
      RPTCURRENCY    = ORIG_CURRENCY
      TIME           = YEAR_INP
      BINARY SEARCH.
      IF SY-SUBRC = 0.
        FACTOR4 = LS_RESOURCE-SIGNEDDATA.
      ELSE.
        FACTOR4 = 0.
      ENDIF.

      IF TIME_STRING = 0 .
        "POST NOTHING

        ELSEIF TIME_STRING < CURR_PERIOD.
        "WHEN RP900003 IS MISSING OR LESS THAN 1ST PERIOD OF THE TIMESCOPE JUST POST ZERO
        LOOP AT LT_TIMESCOPE INTO LS_TIME.
          <LS_SIGNEDDATA> = 0.
          <LS_RESOURCE_ACCT> = 'RP020002' .
          <LS_TIME> = LS_TIME-ID .
          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            "BELOW UPDATING THE LT_RESOURCE_TX START
            CLEAR LS_RESOURCE.
            LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
            LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
            LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
            LS_RESOURCE-ENTITY      = <LS_ENTITY>.
            LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
            LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
            LS_RESOURCE-TIME          = <LS_TIME>.
            LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
            LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
              AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
              AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
              AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
              AND RPTCURRENCY = <LS_RPTCURRENCY> AND
              TIME = <LS_TIME>.
              <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
              ENDLOOP.
              IF SY-SUBRC <> 0.
                INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
                ENDIF.
            "ABOVE UPDATING THE LT_RESOURCE_TX END
          ENDLOOP.

       ELSEIF FACTOR3 = 1 ."RP000400 = 1 MEANS NO INCREASE
         "CHECK IF THERE IS AN INCREASE RP000400
        LOOP AT LT_TIMESCOPE INTO LS_TIME.
          "WHERE TIMEID >= TIME_STRING.
          IF LS_TIME-TIMEID >= TIME_STRING."TO PUT ZERO'S AS PER LUKE
          CLEAR LS_RESOURCE.
          READ TABLE LT_RESOURCE_TX
          INTO LS_RESOURCE
          WITH KEY
          AUDITTRAIL = ORIG_AUDITTRAIL
          CATEGORY   = ORIG_CATEGORY
          EMPLOYEE   = ORIG_EMPLOYEE
          ENTITY     = ORIG_ENTITY
          RESOURCE_ACCT  = 'RP000408'
          RPTCURRENCY    = ORIG_CURRENCY
          TIME           = LS_TIME-ID
          BINARY SEARCH.
          IF SY-SUBRC = 0.
            FACTOR2 = LS_RESOURCE-SIGNEDDATA.
          ELSE.
            FACTOR2 = 0.
          ENDIF.
        <LS_SIGNEDDATA> = ( FACTOR1 / 12 ) * FACTOR2.
        <LS_RESOURCE_ACCT> = 'RP020002' .
        <LS_TIME> = LS_TIME-ID .

        ELSE.
          <LS_SIGNEDDATA> = 0."TO PUT ZERO'S AS PER LUKE
          <LS_RESOURCE_ACCT> = 'RP020002' .
          <LS_TIME> = LS_TIME-ID .
          ENDIF.
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
          "BELOW UPDATING THE LT_RESOURCE_TX START
          CLEAR LS_RESOURCE.
          LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
          LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
          LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
          LS_RESOURCE-ENTITY      = <LS_ENTITY>.
          LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
          LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
          LS_RESOURCE-TIME          = <LS_TIME>.
          LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
          LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
            AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
            AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
            AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
            AND RPTCURRENCY = <LS_RPTCURRENCY> AND
            TIME = <LS_TIME>.
            <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
            ENDLOOP.
            IF SY-SUBRC <> 0.
              INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
              ENDIF.
          "ABOVE UPDATING THE LT_RESOURCE_TX END
        ENDLOOP.
       ELSEIF FACTOR3 <> 1 ."RP000400 <> 1 MEANS THERE IS AN INCREASE EITHER IN 401 OR 402
         "NOW CHECK USE 402 FIRST THEN 401 !!!
         IF FACTOR4 <> 0. "RP000402 <> 0 USE 402.
            READ TABLE LT_RESOURCE_FLG
            INTO LS_RESOURCE
            WITH KEY
            AUDITTRAIL = ORIG_AUDITTRAIL
            CATEGORY   = ORIG_CATEGORY
            EMPLOYEE   = ORIG_EMPLOYEE
            ENTITY     = ORIG_ENTITY
            RESOURCE_ACCT  = 'RP900002' "THIS WILL GIVE YOU YYYYMM IN SIGNEDDATA
            RPTCURRENCY    = ORIG_CURRENCY
            TIME           = YEAR_INP
            BINARY SEARCH.
            IF SY-SUBRC = 0 AND LS_RESOURCE-SIGNEDDATA IS NOT INITIAL.
              FACTOR5 = LS_RESOURCE-SIGNEDDATA.
              ELSE.
              FACTOR5 = 0.
              ENDIF.
           ELSE. "USE 401

            CLEAR LS_RESOURCE.
            READ TABLE LT_RESOURCE_FLG
            INTO LS_RESOURCE
            WITH KEY
            AUDITTRAIL = ORIG_AUDITTRAIL
            CATEGORY   = ORIG_CATEGORY
            EMPLOYEE   = ORIG_EMPLOYEE
            ENTITY     = ORIG_ENTITY
            RESOURCE_ACCT  = 'RP000401' "FACTOR4
            RPTCURRENCY    = ORIG_CURRENCY
            TIME           = YEAR_INP
            BINARY SEARCH.
            IF SY-SUBRC = 0.
              FACTOR4 = LS_RESOURCE-SIGNEDDATA.
            ELSE.
              FACTOR4 = 0.
            ENDIF.

            READ TABLE LT_RESOURCE_FLG
            INTO LS_RESOURCE
            WITH KEY
            AUDITTRAIL = ORIG_AUDITTRAIL
            CATEGORY   = ORIG_CATEGORY
            EMPLOYEE   = ORIG_EMPLOYEE
            ENTITY     = ORIG_ENTITY
            RESOURCE_ACCT  = 'RP900001' "THIS WILL GIVE YOU YYYYMM IN SIGNEDDATA
            RPTCURRENCY    = ORIG_CURRENCY
            TIME           = YEAR_INP
            BINARY SEARCH.
            IF SY-SUBRC = 0 AND LS_RESOURCE-SIGNEDDATA IS NOT INITIAL.
              FACTOR5 = LS_RESOURCE-SIGNEDDATA.
              ELSE.
              FACTOR5 = 0.
              ENDIF.

             ENDIF.

         IF FACTOR4 = 0 OR FACTOR5 = 0."ZERO VALUE ON DATE AND INCREASE
           "JUST POST RP100003 /12 X FTE FROM EFFECTIVE DATE 900003
            LOOP AT LT_TIMESCOPE INTO LS_TIME.
              IF LS_TIME-TIMEID >= TIME_STRING.
                CLEAR LS_RESOURCE.
                READ TABLE LT_RESOURCE_TX
                INTO LS_RESOURCE
                WITH KEY
                AUDITTRAIL = ORIG_AUDITTRAIL
                CATEGORY   = ORIG_CATEGORY
                EMPLOYEE   = ORIG_EMPLOYEE
                ENTITY     = ORIG_ENTITY
                RESOURCE_ACCT  = 'RP000408'
                RPTCURRENCY    = ORIG_CURRENCY
                TIME           = LS_TIME-ID
                BINARY SEARCH.
                IF SY-SUBRC = 0.
                  FACTOR2 = LS_RESOURCE-SIGNEDDATA.
                ELSE.
                  FACTOR2 = 0.
                ENDIF.
                <LS_SIGNEDDATA> = ( FACTOR1 / 12 ) * FACTOR2.
               <LS_RESOURCE_ACCT> = 'RP020002' .
               <LS_TIME> = LS_TIME-ID .
                ELSE.
                <LS_SIGNEDDATA> = 0.
                <LS_RESOURCE_ACCT> = 'RP020002' .
                <LS_TIME> = LS_TIME-ID .
                ENDIF.
               COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
                "BELOW UPDATING THE LT_RESOURCE_TX START
                CLEAR LS_RESOURCE.
                LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
                LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
                LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
                LS_RESOURCE-ENTITY      = <LS_ENTITY>.
                LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
                LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
                LS_RESOURCE-TIME          = <LS_TIME>.
                LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
                LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
                  AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
                  AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
                  AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
                  AND RPTCURRENCY = <LS_RPTCURRENCY> AND
                  TIME = <LS_TIME>.
                  <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
                  ENDLOOP.
                  IF SY-SUBRC <> 0.
                    INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
                    ENDIF.
                "ABOVE UPDATING THE LT_RESOURCE_TX END
              ENDLOOP.
           ELSE. "PROPER VALUE ON FACTOR4 AND FACTOR5
            TIME_STRING1 = FACTOR5.
            TIME_STRING1 = TIME_STRING1(6) && '00'.
            CONDENSE TIME_STRING1 NO-GAPS.
            IMPORT_PERIOD = TIME_STRING1(4) && '.' && TIME_STRING1+4(2).

            CALL METHOD ZCL_BPC_TOOLS_CLASSIC=>PERIOD_OFFSET
              EXPORTING
                IMPORT_PERIOD = IMPORT_PERIOD
                OFFSET        = 11 "BECAUSE WE COUNT INCLUSIVE
              RECEIVING
                EXPORT_PERIOD = EXPORT_PERIOD.  "YYYY.MM

            TIME_STRING2 = EXPORT_PERIOD .
            TIME_STRING2 = TIME_STRING2(4) && TIME_STRING2+5(2) && '00'.
            CONDENSE TIME_STRING2 NO-GAPS. "YYYYMM00

            IMPORT_PERIOD = EXPORT_PERIOD .

            CALL METHOD ZCL_BPC_TOOLS_CLASSIC=>PERIOD_OFFSET
              EXPORTING
                IMPORT_PERIOD = IMPORT_PERIOD
                OFFSET        = 12 "SECOND YEAR
              RECEIVING
                EXPORT_PERIOD = EXPORT_PERIOD.  "YYYY.MM

            TIME_STRING3 = EXPORT_PERIOD .
            TIME_STRING3 = TIME_STRING3(4) && TIME_STRING3+5(2) && '00'.
            CONDENSE TIME_STRING3 NO-GAPS. "YYYYMM00

            LOOP AT LT_TIMESCOPE INTO LS_TIME .
              CLEAR LS_RESOURCE.
              READ TABLE LT_RESOURCE_TX
              INTO LS_RESOURCE
              WITH KEY
              AUDITTRAIL = ORIG_AUDITTRAIL
              CATEGORY   = ORIG_CATEGORY
              EMPLOYEE   = ORIG_EMPLOYEE
              ENTITY     = ORIG_ENTITY
              RESOURCE_ACCT  = 'RP000408' "FTE =>FACTOR2
              RPTCURRENCY    = ORIG_CURRENCY
              TIME           = LS_TIME-ID
              BINARY SEARCH.
              IF SY-SUBRC = 0.
                FACTOR2 = LS_RESOURCE-SIGNEDDATA.
              ELSE.
                FACTOR2 = 0.
              ENDIF.

              "CALCULATE SIGNEDDATA BASED ON LS_TIME-ID
              "POST 0 WHERE THERE IS NO INCREASE
              IF LS_TIME-TIMEID < TIME_STRING.
                <LS_SIGNEDDATA> = 0.
                ELSEIF LS_TIME-TIMEID >= TIME_STRING AND TIME_STRING1 >= TIME_STRING .

                  IF LS_TIME-TIMEID BETWEEN TIME_STRING1 AND TIME_STRING2.
                    <LS_SIGNEDDATA> = ( FACTOR1 / 12 ) * FACTOR2 * ( FACTOR4 + 1 ).
                    ELSEIF LS_TIME-TIMEID > TIME_STRING2.
                      <LS_SIGNEDDATA> = ( FACTOR1 / 12 ) * FACTOR2 * ( FACTOR4 + 1 )
                      * ( FACTOR4 + 1 ).
                      ELSE.
                      <LS_SIGNEDDATA> = ( FACTOR1 / 12 ) * FACTOR2.
                    ENDIF.

                ELSE.

                  IF TIME_STRING <= TIME_STRING2 AND LS_TIME-TIMEID <= TIME_STRING2  .
                    <LS_SIGNEDDATA> = ( FACTOR1 / 12 ) * FACTOR2.
                    ELSEIF TIME_STRING <= TIME_STRING2 AND LS_TIME-TIMEID > TIME_STRING2 .
                      <LS_SIGNEDDATA> = ( FACTOR1 / 12 ) * FACTOR2 * ( FACTOR4 + 1 ) .
                      ELSEIF LS_TIME-TIMEID >= TIME_STRING3.
                        <LS_SIGNEDDATA> = ( FACTOR1 / 12 ) * FACTOR2 * ( FACTOR4 + 1 ) .
                        ELSE.
                          <LS_SIGNEDDATA> = ( FACTOR1 / 12 ) * FACTOR2 .

                      ENDIF.

                  ENDIF.

              "POST TO
              <LS_RESOURCE_ACCT> = 'RP020002' .
              <LS_TIME> = LS_TIME-ID .
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

              "BELOW UPDATING THE LT_RESOURCE_TX START
              CLEAR LS_RESOURCE.
              LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
              LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
              LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
              LS_RESOURCE-ENTITY      = <LS_ENTITY>.
              LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
              LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
              LS_RESOURCE-TIME          = <LS_TIME>.
              LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
              LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
                AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
                AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
                AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
                AND RPTCURRENCY = <LS_RPTCURRENCY> AND
                TIME = <LS_TIME>.
                <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
                ENDLOOP.
                IF SY-SUBRC <> 0.
                  INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
                  ENDIF.
              "ABOVE UPDATING THE LT_RESOURCE_TX END

              ENDLOOP.
             ENDIF.
         ENDIF.



***"POSTING YEARLY VALUES
       "CALCULATE YEAR1 AND YEAR2 VALUE FOR RP020002 AND POST TO YEAR.INP AND YEAR2.INP
       CLEAR: FACTOR7 , FACTOR8,FACTOR10, FACTOR9, FACTOR11.
       LOOP AT LT_TIMESCOPE INTO LS_TIME .
        FACTOR8   = FACTOR7.
        FACTOR10  = FACTOR9.
        READ TABLE LT_RESOURCE_TX
        INTO LS_RESOURCE
        WITH KEY
        AUDITTRAIL = ORIG_AUDITTRAIL
        CATEGORY   = ORIG_CATEGORY
        EMPLOYEE   = ORIG_EMPLOYEE
        ENTITY     = ORIG_ENTITY
        RESOURCE_ACCT  = 'RP020002'
        RPTCURRENCY    = ORIG_CURRENCY
        TIME           = LS_TIME-ID
        BINARY SEARCH.
        IF SY-SUBRC = 0 .
          FACTOR11 = 1.
          "CHECK FOR YEAR1 || YEAR2
          IF LS_RESOURCE-TIME(4) = YEAR_INP(4).
            <LS_RESOURCE_ACCT> = 'RP020002' .
            <LS_TIME> = YEAR_INP .
            <LS_SIGNEDDATA> = LS_RESOURCE-SIGNEDDATA .
            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            FACTOR7 = LS_RESOURCE-SIGNEDDATA + FACTOR8 .
            ELSE.
            <LS_RESOURCE_ACCT> = 'RP020002' .
            <LS_TIME> = YEAR2_INP .
            <LS_SIGNEDDATA> = LS_RESOURCE-SIGNEDDATA .
            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            FACTOR9 = LS_RESOURCE-SIGNEDDATA + FACTOR10 .
            ENDIF.
          ENDIF.
       ENDLOOP.
       "UPDATE LT_RESOURCE_TX
       IF FACTOR11 = 1.
        "BELOW UPDATING THE LT_RESOURCE_TX START
        CLEAR LS_RESOURCE.
        LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
        LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
        LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
        LS_RESOURCE-ENTITY      = <LS_ENTITY>.
        LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
        LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
        LS_RESOURCE-TIME          = YEAR_INP.
        LS_RESOURCE-SIGNEDDATA    = FACTOR7.
        LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
          AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
          AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
          AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
          AND RPTCURRENCY = <LS_RPTCURRENCY> AND
          TIME = YEAR_INP.
          <LS_RESOURCE>-SIGNEDDATA = FACTOR7.
          ENDLOOP.
          IF SY-SUBRC <> 0.
            INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
            ENDIF.
        "ABOVE UPDATING THE LT_RESOURCE_TX END

        "BELOW UPDATING THE LT_RESOURCE_TX START
        CLEAR LS_RESOURCE.
        LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
        LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
        LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
        LS_RESOURCE-ENTITY      = <LS_ENTITY>.
        LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
        LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
        LS_RESOURCE-TIME          = YEAR2_INP.
        LS_RESOURCE-SIGNEDDATA    = FACTOR9.
        LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
          AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
          AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
          AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
          AND RPTCURRENCY = <LS_RPTCURRENCY> AND
          TIME = YEAR2_INP.
          <LS_RESOURCE>-SIGNEDDATA = FACTOR9.
          ENDLOOP.
          IF SY-SUBRC <> 0.
            INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
            ENDIF.
        "ABOVE UPDATING THE LT_RESOURCE_TX END
         ENDIF.

      ENDIF."TEST FOR PERM AND 425
"GEN CALC 4 - Calculate other salary increase monthly values --END

**************************************************************************************************************
" GEN CALC 2 CALCULATE MONTHLY SALARY & BONUS --START

IF ( RESOURCE_TYPE = 'E' AND EMPLOYEE_FLAG = 'Y' )  OR
      ( VAR_RP000425 = 0 AND ROLES_FLAG = 'Y' ). "CALC IS ONLY FOR PERM EMP OR PERM ROLES GEN CALC 2

CLEAR: FACTOR1 , FACTOR2, FACTOR3, FACTOR4 , FACTOR5, FACTOR6
          , FACTOR7, FACTOR8 , FACTOR9 , FACTOR10, FACTOR19, FACTOR20 ,
          TIME_STRING , TIME_STRING2, TIME_STRING3 .

          CLEAR LS_RESOURCE.
          READ TABLE LT_RESOURCE_FLG
          INTO LS_RESOURCE
          WITH KEY
          AUDITTRAIL    = ORIG_AUDITTRAIL
          CATEGORY      = ORIG_CATEGORY
          EMPLOYEE      = ORIG_EMPLOYEE
          ENTITY        = ORIG_ENTITY
          RESOURCE_ACCT  = 'RP010001' "FACTOR1
          RPTCURRENCY    = ORIG_CURRENCY
          TIME           = 'XXXX.INP'
          BINARY SEARCH.
          IF SY-SUBRC = 0.
            FACTOR1 = LS_RESOURCE-SIGNEDDATA .
            ELSE.
              FACTOR1 = 0.
              ENDIF.

          CLEAR LS_RESOURCE.
          READ TABLE LT_RESOURCE_FLG
          INTO LS_RESOURCE
          WITH KEY
          AUDITTRAIL    = ORIG_AUDITTRAIL
          CATEGORY      = ORIG_CATEGORY
          EMPLOYEE      = ORIG_EMPLOYEE
          ENTITY        = ORIG_ENTITY
          RESOURCE_ACCT  = 'RP000400' "FACTOR2
          RPTCURRENCY    = ORIG_CURRENCY
          TIME           = YEAR_INP
          BINARY SEARCH.
          IF SY-SUBRC = 0.
            FACTOR2 = LS_RESOURCE-SIGNEDDATA .
            ELSE.
              FACTOR2 = 0.
              ENDIF.

          CLEAR LS_RESOURCE.
          READ TABLE LT_RESOURCE_FLG
          INTO LS_RESOURCE
          WITH KEY
          AUDITTRAIL    = ORIG_AUDITTRAIL
          CATEGORY      = ORIG_CATEGORY
          EMPLOYEE      = ORIG_EMPLOYEE
          ENTITY        = ORIG_ENTITY
          RESOURCE_ACCT  = 'RP000401' "FACTOR3
          RPTCURRENCY    = ORIG_CURRENCY
          TIME           = YEAR_INP
          BINARY SEARCH.
          IF SY-SUBRC = 0.
            FACTOR3 = LS_RESOURCE-SIGNEDDATA .
            ELSE.
              FACTOR3 = 0.
              ENDIF.

          CLEAR LS_RESOURCE.
          READ TABLE LT_RESOURCE_FLG
          INTO LS_RESOURCE
          WITH KEY
          AUDITTRAIL    = ORIG_AUDITTRAIL
          CATEGORY      = ORIG_CATEGORY
          EMPLOYEE      = ORIG_EMPLOYEE
          ENTITY        = ORIG_ENTITY
          RESOURCE_ACCT  = 'RP000402' "FACTOR4
          RPTCURRENCY    = ORIG_CURRENCY
          TIME           = YEAR_INP
          BINARY SEARCH.
          IF SY-SUBRC = 0.
            FACTOR4 = LS_RESOURCE-SIGNEDDATA .
            ELSE.
              FACTOR4 = 0.
              ENDIF.

          CLEAR LS_RESOURCE.
          READ TABLE LT_RESOURCE_FLG
          INTO LS_RESOURCE
          WITH KEY
          AUDITTRAIL    = ORIG_AUDITTRAIL
          CATEGORY      = ORIG_CATEGORY
          EMPLOYEE      = ORIG_EMPLOYEE
          ENTITY        = ORIG_ENTITY
          RESOURCE_ACCT  = 'RP000404' "FACTOR5
          RPTCURRENCY    = ORIG_CURRENCY
          TIME           = YEAR_INP
          BINARY SEARCH.
          IF SY-SUBRC = 0.
            FACTOR5 = LS_RESOURCE-SIGNEDDATA .
            ELSE.
              FACTOR5 = 0.
              ENDIF.

          CLEAR LS_RESOURCE.
          READ TABLE LT_RESOURCE_FLG
          INTO LS_RESOURCE
          WITH KEY
          AUDITTRAIL    = ORIG_AUDITTRAIL
          CATEGORY      = ORIG_CATEGORY
          EMPLOYEE      = ORIG_EMPLOYEE
          ENTITY        = ORIG_ENTITY
          RESOURCE_ACCT  = 'RP000425' "FACTOR6
          RPTCURRENCY    = ORIG_CURRENCY
          TIME           = 'XXXX.INP'
          BINARY SEARCH.
          IF SY-SUBRC = 0.
            FACTOR6 = LS_RESOURCE-SIGNEDDATA .
            ELSE.
              FACTOR6 = 0.
              ENDIF.

          CLEAR LS_RESOURCE.
          READ TABLE LT_RESOURCE_FLG
          INTO LS_RESOURCE
          WITH KEY
          AUDITTRAIL = ORIG_AUDITTRAIL
          CATEGORY   = ORIG_CATEGORY
          EMPLOYEE   = ORIG_EMPLOYEE
          ENTITY     = ORIG_ENTITY
          RESOURCE_ACCT  = 'RP900001' "THIS WILL GIVE YOU YYYYMM IN SIGNEDDATA
          RPTCURRENCY    = ORIG_CURRENCY
          TIME           = YEAR_INP
          BINARY SEARCH.
          IF SY-SUBRC = 0.
            FACTOR19 = LS_RESOURCE-SIGNEDDATA .
            ELSE.
              FACTOR19 = 0.
              ENDIF.

          CLEAR LS_RESOURCE.
          READ TABLE LT_RESOURCE_FLG
          INTO LS_RESOURCE
          WITH KEY
          AUDITTRAIL = ORIG_AUDITTRAIL
          CATEGORY   = ORIG_CATEGORY
          EMPLOYEE   = ORIG_EMPLOYEE
          ENTITY     = ORIG_ENTITY
          RESOURCE_ACCT  = 'RP900002' "THIS WILL GIVE YOU YYYYMM IN SIGNEDDATA
          RPTCURRENCY    = ORIG_CURRENCY
          TIME           = YEAR_INP
          BINARY SEARCH.
          IF SY-SUBRC = 0.
            FACTOR20 = LS_RESOURCE-SIGNEDDATA .
            ELSE.
              FACTOR20 = 0.
              ENDIF.

        IF FACTOR2 = 1. " 'RP000400' NO INCREASE :(

          LOOP AT LT_TIMESCOPE INTO LS_TIME.
          CLEAR LS_RESOURCE.
          READ TABLE LT_RESOURCE_TX
          INTO LS_RESOURCE
          WITH KEY
          AUDITTRAIL    = ORIG_AUDITTRAIL
          CATEGORY      = ORIG_CATEGORY
          EMPLOYEE      = ORIG_EMPLOYEE
          ENTITY        = ORIG_ENTITY
          RESOURCE_ACCT  = 'RP000408'
          RPTCURRENCY    = ORIG_CURRENCY
          TIME           = LS_TIME-ID
          BINARY SEARCH.
          IF SY-SUBRC = 0.
              "OK
            ELSE.
              LS_RESOURCE-SIGNEDDATA = 0.
              ENDIF.
          <LS_SIGNEDDATA> = ( FACTOR1 / 12 ) * LS_RESOURCE-SIGNEDDATA.
          FACTOR8 = <LS_SIGNEDDATA>.
          <LS_RESOURCE_ACCT> = 'RP020001' .
          <LS_TIME> = LS_TIME-ID .
          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            "BELOW UPDATING THE LT_RESOURCE_TX START
            CLEAR LS_RESOURCE.
            LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
            LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
            LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
            LS_RESOURCE-ENTITY      = <LS_ENTITY>.
            LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
            LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
            LS_RESOURCE-TIME          = <LS_TIME>.
            LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
            LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
              AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
              AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
              AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
              AND RPTCURRENCY = <LS_RPTCURRENCY> AND
              TIME = <LS_TIME>.
              <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
              ENDLOOP.
              IF SY-SUBRC <> 0.
                INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
                ENDIF.
            "ABOVE UPDATING THE LT_RESOURCE_TX END
            READ TABLE LT_RESOURCE_TX INTO LS_RESOURCE
            WITH KEY
            AUDITTRAIL    = ORIG_AUDITTRAIL
            CATEGORY      = ORIG_CATEGORY
            EMPLOYEE      = ORIG_EMPLOYEE
            ENTITY        = ORIG_ENTITY
            RESOURCE_ACCT  = 'RP020002'
            RPTCURRENCY    = ORIG_CURRENCY
            TIME           = LS_TIME-ID
            BINARY SEARCH.
            IF SY-SUBRC = 0.
              FACTOR9 = LS_RESOURCE-SIGNEDDATA .
              ELSE.
                FACTOR9 = 0.
                ENDIF.
            <LS_SIGNEDDATA> = ( FACTOR8 + FACTOR9 ) * FACTOR5 .
            <LS_RESOURCE_ACCT> = 'RP020003' .
            <LS_TIME> = LS_TIME-ID .
            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            "BELOW UPDATING THE LT_RESOURCE_TX START
            CLEAR LS_RESOURCE.
            LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
            LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
            LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
            LS_RESOURCE-ENTITY      = <LS_ENTITY>.
            LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
            LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
            LS_RESOURCE-TIME          = <LS_TIME>.
            LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
            LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
              AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
              AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
              AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
              AND RPTCURRENCY = <LS_RPTCURRENCY> AND
              TIME = <LS_TIME>.
              <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
              ENDLOOP.
              IF SY-SUBRC <> 0.
                INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
                ENDIF.
            "ABOVE UPDATING THE LT_RESOURCE_TX END

            IF REGION <> 'UK'.    "CR3102
            <LS_RESOURCE_ACCT> = 'AC710080' .
            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
              ENDIF.  "CR3102

            ENDLOOP.

          ELSE." RP000400 <> 1   (FACTOR2)

            IF FACTOR20 <> 0 AND FACTOR4 <> 0 .
              "USING 402 AND 900002
            READ TABLE LT_RESOURCE_FLG
            INTO LS_RESOURCE
            WITH KEY
            AUDITTRAIL = ORIG_AUDITTRAIL
            CATEGORY   = ORIG_CATEGORY
            EMPLOYEE   = ORIG_EMPLOYEE
            ENTITY     = ORIG_ENTITY
            RESOURCE_ACCT  = 'RP900002' "THIS WILL GIVE YOU YYYYMM IN SIGNEDDATA
            RPTCURRENCY    = ORIG_CURRENCY
            TIME           = YEAR_INP
            BINARY SEARCH.
            IF SY-SUBRC = 0 AND LS_RESOURCE-SIGNEDDATA IS NOT INITIAL.
              TIME_STRING = LS_RESOURCE-SIGNEDDATA.
              TIME_STRING = TIME_STRING(6) && '00'.
              CONDENSE TIME_STRING NO-GAPS.
              IMPORT_PERIOD = TIME_STRING(4) && '.' && TIME_STRING+4(2).

              CALL METHOD ZCL_BPC_TOOLS_CLASSIC=>PERIOD_OFFSET
                EXPORTING
                  IMPORT_PERIOD = IMPORT_PERIOD
                  OFFSET        = 11 "BECAUSE WE COUNT INCLUSIVE
                RECEIVING
                  EXPORT_PERIOD = EXPORT_PERIOD.  "YYYY.MM

              TIME_STRING2 = EXPORT_PERIOD .
              TIME_STRING2 = TIME_STRING2(4) && TIME_STRING2+5(2) && '00'.
              CONDENSE TIME_STRING2 NO-GAPS. "YYYYMM00

              IMPORT_PERIOD = EXPORT_PERIOD .

              CALL METHOD ZCL_BPC_TOOLS_CLASSIC=>PERIOD_OFFSET
                EXPORTING
                  IMPORT_PERIOD = IMPORT_PERIOD
                  OFFSET        = 11 "BECAUSE WE COUNT INCLUSIVE
                RECEIVING
                  EXPORT_PERIOD = EXPORT_PERIOD.  "YYYY.MM

              TIME_STRING3 = EXPORT_PERIOD .
              TIME_STRING3 = TIME_STRING3(4) && TIME_STRING3+5(2) && '00'.
              CONDENSE TIME_STRING3 NO-GAPS. "YYYYMM00

            ELSE.
              TIME_STRING = 0.
            ENDIF.

               "//CALC SAL BEFORE PAY RISE
               "CHECK IF INC WAS IN THE PAST RP900001 < CURR_PERIOD
               "TIME_STRING < CURR_PERIOD
               IF TIME_STRING < CURR_PERIOD .

               LOOP AT LT_TIMESCOPE INTO LS_TIME .

                READ TABLE LT_RESOURCE_TX INTO LS_RESOURCE
                WITH KEY
                AUDITTRAIL    = ORIG_AUDITTRAIL
                CATEGORY      = ORIG_CATEGORY
                EMPLOYEE      = ORIG_EMPLOYEE
                ENTITY        = ORIG_ENTITY
                RESOURCE_ACCT  = 'RP020002'
                RPTCURRENCY    = ORIG_CURRENCY
                TIME           = LS_TIME-ID
                BINARY SEARCH.
                 IF SY-SUBRC = 0.
                   FACTOR9 = LS_RESOURCE-SIGNEDDATA.
                   ELSE.
                     FACTOR9 = 0.
                     ENDIF.

                CLEAR LS_RESOURCE.
                READ TABLE LT_RESOURCE_TX
                INTO LS_RESOURCE
                WITH KEY
                AUDITTRAIL    = ORIG_AUDITTRAIL
                CATEGORY      = ORIG_CATEGORY
                EMPLOYEE      = ORIG_EMPLOYEE
                ENTITY        = ORIG_ENTITY
                RESOURCE_ACCT  = 'RP000408'
                RPTCURRENCY    = ORIG_CURRENCY
                TIME           = LS_TIME-ID
                BINARY SEARCH.
                IF SY-SUBRC = 0.
                  FACTOR10 = LS_RESOURCE-SIGNEDDATA.
                  ELSE.
                    FACTOR10 = 0.
                    ENDIF.

                  "CALCULATE RECORD
                IF LS_TIME-TIMEID >= TIME_STRING AND LS_TIME-TIMEID <= TIME_STRING2.

                       <LS_SIGNEDDATA> = ( FACTOR1 / 12 ) *  FACTOR10.
                       FACTOR7 =  <LS_SIGNEDDATA>. "SAME AS FACTOR8
                       <LS_RESOURCE_ACCT> = 'RP020001' .
                       <LS_TIME> = LS_TIME-ID .
                       COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

                ELSEIF LS_TIME-TIMEID > TIME_STRING2 AND LS_TIME-TIMEID <= TIME_STRING3.

                       <LS_SIGNEDDATA> = ( FACTOR1 / 12 ) * ( 1 + FACTOR4 ) *  FACTOR10 .
                       FACTOR7 =  <LS_SIGNEDDATA>.
                       <LS_RESOURCE_ACCT> = 'RP020001' .
                       <LS_TIME> = LS_TIME-ID .
                       COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

                ELSEIF LS_TIME-TIMEID > TIME_STRING3.

                       <LS_SIGNEDDATA> = ( FACTOR1 / 12 ) * ( 1 + FACTOR4 ) *
                       ( 1 + FACTOR4 ) *  FACTOR10 .
                       FACTOR7 =  <LS_SIGNEDDATA>.
                       <LS_RESOURCE_ACCT> = 'RP020001' .
                       <LS_TIME> = LS_TIME-ID .
                       COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

                  ENDIF.

                  "BELOW UPDATING THE LT_RESOURCE_TX START
                  CLEAR LS_RESOURCE.
                  LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
                  LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
                  LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
                  LS_RESOURCE-ENTITY      = <LS_ENTITY>.
                  LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
                  LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
                  LS_RESOURCE-TIME          = <LS_TIME>.
                  LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
                  LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
                    AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
                    AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
                    AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
                    AND RPTCURRENCY = <LS_RPTCURRENCY> AND
                    TIME = <LS_TIME>.
                    <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
                    ENDLOOP.
                    IF SY-SUBRC <> 0.
                      INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
                      ENDIF.
                  "ABOVE UPDATING THE LT_RESOURCE_TX END

                 <LS_SIGNEDDATA> = ( FACTOR7 + FACTOR9 ) * FACTOR5  .
                 <LS_RESOURCE_ACCT> = 'RP020003' .
                 <LS_TIME> = LS_TIME-ID .
                 COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            "BELOW UPDATING THE LT_RESOURCE_TX START
            CLEAR LS_RESOURCE.
            LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
            LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
            LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
            LS_RESOURCE-ENTITY      = <LS_ENTITY>.
            LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
            LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
            LS_RESOURCE-TIME          = <LS_TIME>.
            LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
            LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
              AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
              AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
              AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
              AND RPTCURRENCY = <LS_RPTCURRENCY> AND
              TIME = <LS_TIME>.
              <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
              ENDLOOP.
              IF SY-SUBRC <> 0.
                INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
                ENDIF.
            "ABOVE UPDATING THE LT_RESOURCE_TX END
              IF REGION <> 'UK'.    "CR3102
              <LS_RESOURCE_ACCT> = 'AC710080' .
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
                ENDIF.  "CR3102
              ENDLOOP.
               ELSE. "TIME_STRING >= CURR_PERIOD
               "SALARY INCREASE WAS IN TIMESCOPE
               LOOP AT LT_TIMESCOPE INTO LS_TIME .

                READ TABLE LT_RESOURCE_TX INTO LS_RESOURCE
                WITH KEY
                AUDITTRAIL    = ORIG_AUDITTRAIL
                CATEGORY      = ORIG_CATEGORY
                EMPLOYEE      = ORIG_EMPLOYEE
                ENTITY        = ORIG_ENTITY
                RESOURCE_ACCT  = 'RP020002'
                RPTCURRENCY    = ORIG_CURRENCY
                TIME           = LS_TIME-ID
                BINARY SEARCH.
                 IF SY-SUBRC = 0.
                   FACTOR9 = LS_RESOURCE-SIGNEDDATA.
                   ELSE.
                     FACTOR9 = 0.
                     ENDIF.

                CLEAR LS_RESOURCE.
                READ TABLE LT_RESOURCE_TX
                INTO LS_RESOURCE
                WITH KEY
                AUDITTRAIL    = ORIG_AUDITTRAIL
                CATEGORY      = ORIG_CATEGORY
                EMPLOYEE      = ORIG_EMPLOYEE
                ENTITY        = ORIG_ENTITY
                RESOURCE_ACCT  = 'RP000408'
                RPTCURRENCY    = ORIG_CURRENCY
                TIME           = LS_TIME-ID
                BINARY SEARCH.
                IF SY-SUBRC = 0.
                  FACTOR10 = LS_RESOURCE-SIGNEDDATA.
                  ELSE.
                    FACTOR10 = 0.
                    ENDIF.

                  "CALCULATE RECORD
             IF LS_TIME-TIMEID < TIME_STRING.

                 <LS_SIGNEDDATA> = ( FACTOR1 / 12 ) *  FACTOR10.
                 FACTOR7 =  <LS_SIGNEDDATA>. "SAME AS FACTOR8
                 <LS_RESOURCE_ACCT> = 'RP020001' .
                 <LS_TIME> = LS_TIME-ID .
                 COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

              ELSEIF LS_TIME-TIMEID >= TIME_STRING AND LS_TIME-TIMEID <= TIME_STRING2.

                 <LS_SIGNEDDATA> = ( FACTOR1 / 12 ) * ( 1 + FACTOR4 ) *  FACTOR10 .
                 FACTOR7 =  <LS_SIGNEDDATA>.
                 <LS_RESOURCE_ACCT> = 'RP020001' .
                 <LS_TIME> = LS_TIME-ID .
                 COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

              ELSEIF LS_TIME-TIMEID > TIME_STRING2.

                 <LS_SIGNEDDATA> = ( FACTOR1 / 12 ) * ( 1 + FACTOR4 ) *
                 ( 1 + FACTOR4 ) *  FACTOR10 .
                 FACTOR7 =  <LS_SIGNEDDATA>.
                 <LS_RESOURCE_ACCT> = 'RP020001' .
                 <LS_TIME> = LS_TIME-ID .
                 COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

              ENDIF.

                  "BELOW UPDATING THE LT_RESOURCE_TX START
                  CLEAR LS_RESOURCE.
                  LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
                  LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
                  LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
                  LS_RESOURCE-ENTITY      = <LS_ENTITY>.
                  LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
                  LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
                  LS_RESOURCE-TIME          = <LS_TIME>.
                  LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
                  LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
                    AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
                    AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
                    AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
                    AND RPTCURRENCY = <LS_RPTCURRENCY> AND
                    TIME = <LS_TIME>.
                    <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
                    ENDLOOP.
                    IF SY-SUBRC <> 0.
                      INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
                      ENDIF.
                  "ABOVE UPDATING THE LT_RESOURCE_TX END

                 <LS_SIGNEDDATA> = ( FACTOR7 + FACTOR9 ) * FACTOR5  .
                 <LS_RESOURCE_ACCT> = 'RP020003' .
                 <LS_TIME> = LS_TIME-ID .
                 COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            "BELOW UPDATING THE LT_RESOURCE_TX START
            CLEAR LS_RESOURCE.
            LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
            LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
            LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
            LS_RESOURCE-ENTITY      = <LS_ENTITY>.
            LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
            LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
            LS_RESOURCE-TIME          = <LS_TIME>.
            LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
            LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
              AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
              AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
              AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
              AND RPTCURRENCY = <LS_RPTCURRENCY> AND
              TIME = <LS_TIME>.
              <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
              ENDLOOP.
              IF SY-SUBRC <> 0.
                INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
                ENDIF.
            "ABOVE UPDATING THE LT_RESOURCE_TX END
                IF REGION <> 'UK'.    "CR3102
                <LS_RESOURCE_ACCT> = 'AC710080' .
                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
                  ENDIF.  "CR3102
              ENDLOOP.

              ENDIF. "TIME_STRING < CURR_PERIOD


              ELSEIF ( FACTOR20 <> 0 OR FACTOR4 <> 0 )  AND ( FACTOR20 = 0 OR FACTOR4 = 0 ).
                "CALCULATE WITH NO INCREASE
                "CALCULATE WITH NO INCREASE
                LOOP AT LT_TIMESCOPE INTO LS_TIME . "TIME_STRING =  0

                  READ TABLE LT_RESOURCE_TX INTO LS_RESOURCE
                  WITH KEY
                  AUDITTRAIL    = ORIG_AUDITTRAIL
                  CATEGORY      = ORIG_CATEGORY
                  EMPLOYEE      = ORIG_EMPLOYEE
                  ENTITY        = ORIG_ENTITY
                  RESOURCE_ACCT  = 'RP020002'
                  RPTCURRENCY    = ORIG_CURRENCY
                  TIME           = LS_TIME-ID
                  BINARY SEARCH.
                   IF SY-SUBRC = 0.
                     FACTOR9 = LS_RESOURCE-SIGNEDDATA.
                     ELSE.
                       FACTOR9 = 0.
                       ENDIF.

                  CLEAR LS_RESOURCE.
                  READ TABLE LT_RESOURCE_TX
                  INTO LS_RESOURCE
                  WITH KEY
                  AUDITTRAIL    = ORIG_AUDITTRAIL
                  CATEGORY      = ORIG_CATEGORY
                  EMPLOYEE      = ORIG_EMPLOYEE
                  ENTITY        = ORIG_ENTITY
                  RESOURCE_ACCT  = 'RP000408'
                  RPTCURRENCY    = ORIG_CURRENCY
                  TIME           = LS_TIME-ID
                  BINARY SEARCH.
                  IF SY-SUBRC = 0.
                    FACTOR10 = LS_RESOURCE-SIGNEDDATA.
                    ELSE.
                      FACTOR10 = 0.
                      ENDIF.

                  <LS_SIGNEDDATA> = ( FACTOR1 / 12 ) *  FACTOR10.
                  FACTOR7 =  <LS_SIGNEDDATA>. "SAME AS FACTOR8
                  <LS_RESOURCE_ACCT> = 'RP020001' .
                  <LS_TIME> = LS_TIME-ID .
                  COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

                    "BELOW UPDATING THE LT_RESOURCE_TX START
                    CLEAR LS_RESOURCE.
                    LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
                    LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
                    LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
                    LS_RESOURCE-ENTITY      = <LS_ENTITY>.
                    LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
                    LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
                    LS_RESOURCE-TIME          = <LS_TIME>.
                    LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
                    LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
                      AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
                      AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
                      AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
                      AND RPTCURRENCY = <LS_RPTCURRENCY> AND
                      TIME = <LS_TIME>.
                      <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
                      ENDLOOP.
                      IF SY-SUBRC <> 0.
                        INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
                        ENDIF.
                       "ABOVE UPDATING THE LT_RESOURCE_TX END
                           <LS_SIGNEDDATA> = ( FACTOR7 + FACTOR9 ) * FACTOR5  .
                           <LS_RESOURCE_ACCT> = 'RP020003' .
                           <LS_TIME> = LS_TIME-ID .
                           COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
                      "BELOW UPDATING THE LT_RESOURCE_TX START
                      CLEAR LS_RESOURCE.
                      LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
                      LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
                      LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
                      LS_RESOURCE-ENTITY      = <LS_ENTITY>.
                      LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
                      LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
                      LS_RESOURCE-TIME          = <LS_TIME>.
                      LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
                      LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
                        AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
                        AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
                        AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
                        AND RPTCURRENCY = <LS_RPTCURRENCY> AND
                        TIME = <LS_TIME>.
                        <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
                        ENDLOOP.
                        IF SY-SUBRC <> 0.
                          INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
                          ENDIF.
                      "ABOVE UPDATING THE LT_RESOURCE_TX END
                          IF REGION <> 'UK'.    "CR3102
                          <LS_RESOURCE_ACCT> = 'AC710080' .
                          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
                            ENDIF.  "CR3102
                 ENDLOOP. "TIME_STRING =  0
              ELSEIF FACTOR19 <> 0 AND FACTOR3 <> 0.
               "USE 401 AND 900001

            READ TABLE LT_RESOURCE_FLG
            INTO LS_RESOURCE
            WITH KEY
            AUDITTRAIL = ORIG_AUDITTRAIL
            CATEGORY   = ORIG_CATEGORY
            EMPLOYEE   = ORIG_EMPLOYEE
            ENTITY     = ORIG_ENTITY
            RESOURCE_ACCT  = 'RP900001' "THIS WILL GIVE YOU YYYYMM IN SIGNEDDATA
            RPTCURRENCY    = ORIG_CURRENCY
            TIME           = YEAR_INP
            BINARY SEARCH.
            IF SY-SUBRC = 0 AND LS_RESOURCE-SIGNEDDATA IS NOT INITIAL.
              TIME_STRING = LS_RESOURCE-SIGNEDDATA.
              TIME_STRING = TIME_STRING(6) && '00'.
              CONDENSE TIME_STRING NO-GAPS.
              IMPORT_PERIOD = TIME_STRING(4) && '.' && TIME_STRING+4(2).

              CALL METHOD ZCL_BPC_TOOLS_CLASSIC=>PERIOD_OFFSET
                EXPORTING
                  IMPORT_PERIOD = IMPORT_PERIOD
                  OFFSET        = 11 "BECAUSE WE COUNT INCLUSIVE
                RECEIVING
                  EXPORT_PERIOD = EXPORT_PERIOD.  "YYYY.MM

              TIME_STRING2 = EXPORT_PERIOD .
              TIME_STRING2 = TIME_STRING2(4) && TIME_STRING2+5(2) && '00'.
              CONDENSE TIME_STRING2 NO-GAPS. "YYYYMM00

              IMPORT_PERIOD = EXPORT_PERIOD .

              CALL METHOD ZCL_BPC_TOOLS_CLASSIC=>PERIOD_OFFSET
                EXPORTING
                  IMPORT_PERIOD = IMPORT_PERIOD
                  OFFSET        = 12 "THIS IS THE 2ND YEAR HENCE 12
                RECEIVING
                  EXPORT_PERIOD = EXPORT_PERIOD.  "YYYY.MM

              TIME_STRING3 = EXPORT_PERIOD .
              TIME_STRING3 = TIME_STRING3(4) && TIME_STRING3+5(2) && '00'.
              CONDENSE TIME_STRING3 NO-GAPS. "YYYYMM00

            ELSE.
              TIME_STRING = 0.
            ENDIF.

               "//CALC SAL BEFORE PAY RISE
               "CHECK IF INC WAS IN THE PAST RP900001 < CURR_PERIOD
               "TIME_STRING < CURR_PERIOD
               IF TIME_STRING < CURR_PERIOD .

               LOOP AT LT_TIMESCOPE INTO LS_TIME .

                READ TABLE LT_RESOURCE_TX INTO LS_RESOURCE
                WITH KEY
                AUDITTRAIL    = ORIG_AUDITTRAIL
                CATEGORY      = ORIG_CATEGORY
                EMPLOYEE      = ORIG_EMPLOYEE
                ENTITY        = ORIG_ENTITY
                RESOURCE_ACCT  = 'RP020002'
                RPTCURRENCY    = ORIG_CURRENCY
                TIME           = LS_TIME-ID
                BINARY SEARCH.
                 IF SY-SUBRC = 0.
                   FACTOR9 = LS_RESOURCE-SIGNEDDATA.
                   ELSE.
                     FACTOR9 = 0.
                     ENDIF.

                CLEAR LS_RESOURCE.
                READ TABLE LT_RESOURCE_TX
                INTO LS_RESOURCE
                WITH KEY
                AUDITTRAIL    = ORIG_AUDITTRAIL
                CATEGORY      = ORIG_CATEGORY
                EMPLOYEE      = ORIG_EMPLOYEE
                ENTITY        = ORIG_ENTITY
                RESOURCE_ACCT  = 'RP000408'
                RPTCURRENCY    = ORIG_CURRENCY
                TIME           = LS_TIME-ID
                BINARY SEARCH.
                IF SY-SUBRC = 0.
                  FACTOR10 = LS_RESOURCE-SIGNEDDATA.
                  ELSE.
                    FACTOR10 = 0.
                    ENDIF.

                  "CALCULATE RECORD
                IF LS_TIME-TIMEID >= TIME_STRING AND LS_TIME-TIMEID <= TIME_STRING2.

                       <LS_SIGNEDDATA> = ( FACTOR1 / 12 ) *  FACTOR10.
                       FACTOR7 =  <LS_SIGNEDDATA>. "SAME AS FACTOR8
                       <LS_RESOURCE_ACCT> = 'RP020001' .
                       <LS_TIME> = LS_TIME-ID .
                       COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

                ELSEIF LS_TIME-TIMEID > TIME_STRING2 AND LS_TIME-TIMEID <= TIME_STRING3.

                       <LS_SIGNEDDATA> = ( FACTOR1 / 12 ) * ( 1 + FACTOR3 ) *  FACTOR10 .
                       FACTOR7 =  <LS_SIGNEDDATA>.
                       <LS_RESOURCE_ACCT> = 'RP020001' .
                       <LS_TIME> = LS_TIME-ID .
                       COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

                ELSEIF LS_TIME-TIMEID > TIME_STRING3.

                       <LS_SIGNEDDATA> = ( FACTOR1 / 12 ) * ( 1 + FACTOR3 ) *
                       ( 1 + FACTOR3 ) *  FACTOR10 .
                       FACTOR7 =  <LS_SIGNEDDATA>.
                       <LS_RESOURCE_ACCT> = 'RP020001' .
                       <LS_TIME> = LS_TIME-ID .
                       COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

                  ENDIF.

                  "BELOW UPDATING THE LT_RESOURCE_TX START
                  CLEAR LS_RESOURCE.
                  LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
                  LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
                  LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
                  LS_RESOURCE-ENTITY      = <LS_ENTITY>.
                  LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
                  LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
                  LS_RESOURCE-TIME          = <LS_TIME>.
                  LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
                  LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
                    AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
                    AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
                    AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
                    AND RPTCURRENCY = <LS_RPTCURRENCY> AND
                    TIME = <LS_TIME>.
                    <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
                    ENDLOOP.
                    IF SY-SUBRC <> 0.
                      INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
                      ENDIF.
                  "ABOVE UPDATING THE LT_RESOURCE_TX END

                 <LS_SIGNEDDATA> = ( FACTOR7 + FACTOR9 ) * FACTOR5  .
                 <LS_RESOURCE_ACCT> = 'RP020003' .
                 <LS_TIME> = LS_TIME-ID .
                 COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            "BELOW UPDATING THE LT_RESOURCE_TX START
            CLEAR LS_RESOURCE.
            LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
            LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
            LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
            LS_RESOURCE-ENTITY      = <LS_ENTITY>.
            LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
            LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
            LS_RESOURCE-TIME          = <LS_TIME>.
            LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
            LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
              AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
              AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
              AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
              AND RPTCURRENCY = <LS_RPTCURRENCY> AND
              TIME = <LS_TIME>.
              <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
              ENDLOOP.
              IF SY-SUBRC <> 0.
                INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
                ENDIF.
            "ABOVE UPDATING THE LT_RESOURCE_TX END

                  IF REGION <> 'UK'.    "CR3102
                  <LS_RESOURCE_ACCT> = 'AC710080' .
                  COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
                    ENDIF.  "CR3102

              ENDLOOP.
               ELSE. "TIME_STRING >= CURR_PERIOD
               "SALARY INCREASE WAS IN TIMESCOPE
               LOOP AT LT_TIMESCOPE INTO LS_TIME .

                READ TABLE LT_RESOURCE_TX INTO LS_RESOURCE
                WITH KEY
                AUDITTRAIL    = ORIG_AUDITTRAIL
                CATEGORY      = ORIG_CATEGORY
                EMPLOYEE      = ORIG_EMPLOYEE
                ENTITY        = ORIG_ENTITY
                RESOURCE_ACCT  = 'RP020002'
                RPTCURRENCY    = ORIG_CURRENCY
                TIME           = LS_TIME-ID
                BINARY SEARCH.
                 IF SY-SUBRC = 0.
                   FACTOR9 = LS_RESOURCE-SIGNEDDATA.
                   ELSE.
                     FACTOR9 = 0.
                     ENDIF.

                CLEAR LS_RESOURCE.
                READ TABLE LT_RESOURCE_TX
                INTO LS_RESOURCE
                WITH KEY
                AUDITTRAIL    = ORIG_AUDITTRAIL
                CATEGORY      = ORIG_CATEGORY
                EMPLOYEE      = ORIG_EMPLOYEE
                ENTITY        = ORIG_ENTITY
                RESOURCE_ACCT  = 'RP000408'
                RPTCURRENCY    = ORIG_CURRENCY
                TIME           = LS_TIME-ID
                BINARY SEARCH.
                IF SY-SUBRC = 0.
                  FACTOR10 = LS_RESOURCE-SIGNEDDATA.
                  ELSE.
                    FACTOR10 = 0.
                    ENDIF.

                  "CALCULATE RECORD
             IF LS_TIME-TIMEID < TIME_STRING.

                 <LS_SIGNEDDATA> = ( FACTOR1 / 12 ) *  FACTOR10.
                 FACTOR7 =  <LS_SIGNEDDATA>. "SAME AS FACTOR8
                 <LS_RESOURCE_ACCT> = 'RP020001' .
                 <LS_TIME> = LS_TIME-ID .
                 COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

              ELSEIF LS_TIME-TIMEID >= TIME_STRING AND LS_TIME-TIMEID <= TIME_STRING2.

                 <LS_SIGNEDDATA> = ( FACTOR1 / 12 ) * ( 1 + FACTOR3 ) *  FACTOR10 .
                 FACTOR7 =  <LS_SIGNEDDATA>.
                 <LS_RESOURCE_ACCT> = 'RP020001' .
                 <LS_TIME> = LS_TIME-ID .
                 COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

              ELSEIF LS_TIME-TIMEID > TIME_STRING2.

                 <LS_SIGNEDDATA> = ( FACTOR1 / 12 ) * ( 1 + FACTOR3 ) *
                 ( 1 + FACTOR3 ) *  FACTOR10 .
                 FACTOR7 =  <LS_SIGNEDDATA>.
                 <LS_RESOURCE_ACCT> = 'RP020001' .
                 <LS_TIME> = LS_TIME-ID .
                 COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

              ENDIF.

                  "BELOW UPDATING THE LT_RESOURCE_TX START
                  CLEAR LS_RESOURCE.
                  LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
                  LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
                  LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
                  LS_RESOURCE-ENTITY      = <LS_ENTITY>.
                  LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
                  LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
                  LS_RESOURCE-TIME          = <LS_TIME>.
                  LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
                  LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
                    AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
                    AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
                    AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
                    AND RPTCURRENCY = <LS_RPTCURRENCY> AND
                    TIME = <LS_TIME>.
                    <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
                    ENDLOOP.
                    IF SY-SUBRC <> 0.
                      INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
                      ENDIF.
                  "ABOVE UPDATING THE LT_RESOURCE_TX END

                 <LS_SIGNEDDATA> = ( FACTOR7 + FACTOR9 ) * FACTOR5  .
                 <LS_RESOURCE_ACCT> = 'RP020003' .
                 <LS_TIME> = LS_TIME-ID .
                 COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            "BELOW UPDATING THE LT_RESOURCE_TX START
            CLEAR LS_RESOURCE.
            LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
            LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
            LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
            LS_RESOURCE-ENTITY      = <LS_ENTITY>.
            LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
            LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
            LS_RESOURCE-TIME          = <LS_TIME>.
            LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
            LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
              AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
              AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
              AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
              AND RPTCURRENCY = <LS_RPTCURRENCY> AND
              TIME = <LS_TIME>.
              <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
              ENDLOOP.
              IF SY-SUBRC <> 0.
                INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
                ENDIF.
            "ABOVE UPDATING THE LT_RESOURCE_TX END

                IF REGION <> 'UK'.    "CR3102
                <LS_RESOURCE_ACCT> = 'AC710080' .
                COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
                  ENDIF.  "CR3102



              ENDLOOP.

               ENDIF. "TIME_STRING < CURR_PERIOD


              ELSEIF ( FACTOR19 = 0 OR FACTOR3 = 0 ). "( FACTOR19 <> 0 OR FACTOR3 <> 0 )  AND ( FACTOR19 = 0 OR FACTOR3 = 0 ).
                "CALCULATE WITH NO INCREASE
                LOOP AT LT_TIMESCOPE INTO LS_TIME . "TIME_STRING =  0

                  READ TABLE LT_RESOURCE_TX INTO LS_RESOURCE
                  WITH KEY
                  AUDITTRAIL    = ORIG_AUDITTRAIL
                  CATEGORY      = ORIG_CATEGORY
                  EMPLOYEE      = ORIG_EMPLOYEE
                  ENTITY        = ORIG_ENTITY
                  RESOURCE_ACCT  = 'RP020002'
                  RPTCURRENCY    = ORIG_CURRENCY
                  TIME           = LS_TIME-ID
                  BINARY SEARCH.
                   IF SY-SUBRC = 0.
                     FACTOR9 = LS_RESOURCE-SIGNEDDATA.
                     ELSE.
                       FACTOR9 = 0.
                       ENDIF.

                  CLEAR LS_RESOURCE.
                  READ TABLE LT_RESOURCE_TX
                  INTO LS_RESOURCE
                  WITH KEY
                  AUDITTRAIL    = ORIG_AUDITTRAIL
                  CATEGORY      = ORIG_CATEGORY
                  EMPLOYEE      = ORIG_EMPLOYEE
                  ENTITY        = ORIG_ENTITY
                  RESOURCE_ACCT  = 'RP000408'
                  RPTCURRENCY    = ORIG_CURRENCY
                  TIME           = LS_TIME-ID
                  BINARY SEARCH.
                  IF SY-SUBRC = 0.
                    FACTOR10 = LS_RESOURCE-SIGNEDDATA.
                    ELSE.
                      FACTOR10 = 0.
                      ENDIF.

                  <LS_SIGNEDDATA> = ( FACTOR1 / 12 ) *  FACTOR10.
                  FACTOR7 =  <LS_SIGNEDDATA>. "SAME AS FACTOR8
                  <LS_RESOURCE_ACCT> = 'RP020001' .
                  <LS_TIME> = LS_TIME-ID .
                  COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

                    "BELOW UPDATING THE LT_RESOURCE_TX START
                    CLEAR LS_RESOURCE.
                    LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
                    LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
                    LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
                    LS_RESOURCE-ENTITY      = <LS_ENTITY>.
                    LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
                    LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
                    LS_RESOURCE-TIME          = <LS_TIME>.
                    LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
                    LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
                      AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
                      AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
                      AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
                      AND RPTCURRENCY = <LS_RPTCURRENCY> AND
                      TIME = <LS_TIME>.
                      <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
                      ENDLOOP.
                      IF SY-SUBRC <> 0.
                        INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
                        ENDIF.
                       "ABOVE UPDATING THE LT_RESOURCE_TX END
                           <LS_SIGNEDDATA> = ( FACTOR7 + FACTOR9 ) * FACTOR5  .
                           <LS_RESOURCE_ACCT> = 'RP020003' .
                           <LS_TIME> = LS_TIME-ID .
                           COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
                      "BELOW UPDATING THE LT_RESOURCE_TX START
                      CLEAR LS_RESOURCE.
                      LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
                      LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
                      LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
                      LS_RESOURCE-ENTITY      = <LS_ENTITY>.
                      LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
                      LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
                      LS_RESOURCE-TIME          = <LS_TIME>.
                      LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
                      LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
                        AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
                        AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
                        AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
                        AND RPTCURRENCY = <LS_RPTCURRENCY> AND
                        TIME = <LS_TIME>.
                        <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
                        ENDLOOP.
                        IF SY-SUBRC <> 0.
                          INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
                          ENDIF.
                      "ABOVE UPDATING THE LT_RESOURCE_TX END

                          IF REGION <> 'UK'.    "CR3102
                          <LS_RESOURCE_ACCT> = 'AC710080' .
                          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
                            ENDIF.  "CR3102


                 ENDLOOP. "TIME_STRING =  0
              ENDIF. "EVAL OF 402 AND 900002

          ENDIF." 'RP000400' OF WFORECAST

     ELSEIF VAR_RP000425 = 1 AND ROLES_FLAG = 'Y'  .

        READ TABLE LT_TOTAL_ROLES TRANSPORTING NO FIELDS
        WITH KEY TABLE_LINE = ORIG_EMPLOYEE.
        IF SY-SUBRC = 0.

          CLEAR LS_RESOURCE.
          READ TABLE LT_RESOURCE_FLG
          INTO LS_RESOURCE
          WITH KEY
          AUDITTRAIL    = ORIG_AUDITTRAIL
          CATEGORY      = ORIG_CATEGORY
          EMPLOYEE      = ORIG_EMPLOYEE
          ENTITY        = ORIG_ENTITY
          RESOURCE_ACCT  = 'RP010001' "FACTOR1
          RPTCURRENCY    = ORIG_CURRENCY
          TIME           = 'XXXX.INP'
          BINARY SEARCH.
          IF SY-SUBRC = 0.
            FACTOR1 = LS_RESOURCE-SIGNEDDATA .
            ELSE.
              FACTOR1 = 0.
              ENDIF.

          LOOP AT LT_TIMESCOPE INTO LS_TIME.
            CLEAR LS_RESOURCE.
            READ TABLE LT_RESOURCE_TX
            INTO LS_RESOURCE
            WITH KEY
            AUDITTRAIL    = ORIG_AUDITTRAIL
            CATEGORY      = ORIG_CATEGORY
            EMPLOYEE      = ORIG_EMPLOYEE
            ENTITY        = ORIG_ENTITY
            RESOURCE_ACCT  = 'RP000408'
            RPTCURRENCY    = ORIG_CURRENCY
            TIME           = LS_TIME-ID
            BINARY SEARCH.
            IF SY-SUBRC = 0.
              FACTOR10 = LS_RESOURCE-SIGNEDDATA.
              ELSE.
                FACTOR10 = 0.
                ENDIF.

            <LS_SIGNEDDATA> = ( FACTOR1 / 12 ) * FACTOR10.
            <LS_RESOURCE_ACCT> = 'RP020001' .
            <LS_TIME> = LS_TIME-ID .
            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

             "BELOW UPDATING THE LT_RESOURCE_TX START
             CLEAR LS_RESOURCE.
             LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
             LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
             LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
             LS_RESOURCE-ENTITY      = <LS_ENTITY>.
             LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
             LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
             LS_RESOURCE-TIME          = <LS_TIME>.
             LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
             LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
               AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
               AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
               AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
               AND RPTCURRENCY = <LS_RPTCURRENCY> AND
               TIME = <LS_TIME>.
               <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
               ENDLOOP.
               IF SY-SUBRC <> 0.
                 INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
                 ENDIF.
             "ABOVE UPDATING THE LT_RESOURCE_TX END
            ENDLOOP.

            ENDIF.

      ELSEIF RESOURCE_TYPE = 'C' AND EMPLOYEE_FLAG = 'Y'.

          CLEAR LS_RESOURCE.
          READ TABLE LT_RESOURCE_FLG
          INTO LS_RESOURCE
          WITH KEY
          AUDITTRAIL    = ORIG_AUDITTRAIL
          CATEGORY      = ORIG_CATEGORY
          EMPLOYEE      = ORIG_EMPLOYEE
          ENTITY        = ORIG_ENTITY
          RESOURCE_ACCT  = 'RP010001' "FACTOR1
          RPTCURRENCY    = ORIG_CURRENCY
          TIME           = 'XXXX.INP'
          BINARY SEARCH.
          IF SY-SUBRC = 0.
            FACTOR1 = LS_RESOURCE-SIGNEDDATA .
            ELSE.
              FACTOR1 = 0.
              ENDIF.

          LOOP AT LT_TIMESCOPE INTO LS_TIME.
            CLEAR LS_RESOURCE.
            READ TABLE LT_RESOURCE_TX
            INTO LS_RESOURCE
            WITH KEY
            AUDITTRAIL    = ORIG_AUDITTRAIL
            CATEGORY      = ORIG_CATEGORY
            EMPLOYEE      = ORIG_EMPLOYEE
            ENTITY        = ORIG_ENTITY
            RESOURCE_ACCT  = 'RP000408'
            RPTCURRENCY    = ORIG_CURRENCY
            TIME           = LS_TIME-ID
            BINARY SEARCH.
            IF SY-SUBRC = 0.
              FACTOR10 = LS_RESOURCE-SIGNEDDATA.
              ELSE.
                FACTOR10 = 0.
                ENDIF.

            <LS_SIGNEDDATA> = ( FACTOR1 / 12 ) * FACTOR10 .
            <LS_RESOURCE_ACCT> = 'RP020001' .
            <LS_TIME> = LS_TIME-ID .
            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

             "BELOW UPDATING THE LT_RESOURCE_TX START
             CLEAR LS_RESOURCE.
             LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
             LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
             LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
             LS_RESOURCE-ENTITY      = <LS_ENTITY>.
             LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
             LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
             LS_RESOURCE-TIME          = <LS_TIME>.
             LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
             LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
               AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
               AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
               AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
               AND RPTCURRENCY = <LS_RPTCURRENCY> AND
               TIME = <LS_TIME>.
               <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
               ENDLOOP.
               IF SY-SUBRC <> 0.
                 INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
                 ENDIF.
             "ABOVE UPDATING THE LT_RESOURCE_TX END
            ENDLOOP.

      ENDIF. "CALC IS ONLY FOR PERM EMP OR PERM ROLES GEN CALC 2

       "CALCULATE YEAR1 AND YEAR2 VALUE FOR RP020002 AND POST TO YEAR.INP AND YEAR2.INP

IF ( RESOURCE_TYPE = 'E' AND EMPLOYEE_FLAG = 'Y' )  OR ( VAR_RP000425 = 0 AND ROLES_FLAG = 'Y' )
  OR ( VAR_RP000425 = 1 AND ROLES_FLAG = 'Y' ) OR ( RESOURCE_TYPE = 'C' AND EMPLOYEE_FLAG = 'Y' ) .
       CLEAR: FACTOR7 , FACTOR8,FACTOR10, FACTOR9, FACTOR11.
       LOOP AT LT_TIMESCOPE INTO LS_TIME .
        FACTOR8   = FACTOR7.
        FACTOR10  = FACTOR9.
        READ TABLE LT_RESOURCE_TX
        INTO LS_RESOURCE
        WITH KEY
        AUDITTRAIL = ORIG_AUDITTRAIL
        CATEGORY   = ORIG_CATEGORY
        EMPLOYEE   = ORIG_EMPLOYEE
        ENTITY     = ORIG_ENTITY
        RESOURCE_ACCT  = 'RP020001'
        RPTCURRENCY    = ORIG_CURRENCY
        TIME           = LS_TIME-ID
        BINARY SEARCH.
        IF SY-SUBRC = 0 .
          FACTOR11 = 1.
          "CHECK FOR YEAR1 || YEAR2
          IF LS_RESOURCE-TIME(4) = YEAR_INP(4).
            <LS_RESOURCE_ACCT> = 'RP020001' .
            <LS_TIME> = YEAR_INP .
            <LS_SIGNEDDATA> = LS_RESOURCE-SIGNEDDATA .
            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            FACTOR7 = LS_RESOURCE-SIGNEDDATA + FACTOR8 .
            ELSE.
            <LS_RESOURCE_ACCT> = 'RP020001' .
            <LS_TIME> = YEAR2_INP .
            <LS_SIGNEDDATA> = LS_RESOURCE-SIGNEDDATA .
            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            FACTOR9 = LS_RESOURCE-SIGNEDDATA + FACTOR10 .
            ENDIF.
          ENDIF.
       ENDLOOP.
       "UPDATE LT_RESOURCE_TX
       IF FACTOR11 = 1.
        "BELOW UPDATING THE LT_RESOURCE_TX START
        CLEAR LS_RESOURCE.
        LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
        LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
        LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
        LS_RESOURCE-ENTITY      = <LS_ENTITY>.
        LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
        LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
        LS_RESOURCE-TIME          = YEAR_INP.
        LS_RESOURCE-SIGNEDDATA    = FACTOR7.
        LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
          AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
          AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
          AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
          AND RPTCURRENCY = <LS_RPTCURRENCY> AND
          TIME = YEAR_INP.
          <LS_RESOURCE>-SIGNEDDATA = FACTOR7.
          ENDLOOP.
          IF SY-SUBRC <> 0.
            INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
            ENDIF.
        "ABOVE UPDATING THE LT_RESOURCE_TX END

        "BELOW UPDATING THE LT_RESOURCE_TX START
        CLEAR LS_RESOURCE.
        LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
        LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
        LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
        LS_RESOURCE-ENTITY      = <LS_ENTITY>.
        LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
        LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
        LS_RESOURCE-TIME          = YEAR2_INP.
        LS_RESOURCE-SIGNEDDATA    = FACTOR9.
        LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
          AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
          AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
          AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
          AND RPTCURRENCY = <LS_RPTCURRENCY> AND
          TIME = YEAR2_INP.
          <LS_RESOURCE>-SIGNEDDATA = FACTOR9.
          ENDLOOP.
          IF SY-SUBRC <> 0.
            INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
            ENDIF.
        "ABOVE UPDATING THE LT_RESOURCE_TX END
         ENDIF.
ENDIF.

      "GEN CALC 2 CALCULATE MONTHLY SALARY & BONUS --END
********************************************************************************************

"GEN CALC 5 CALCULATE OTHER MONTHLY VALUES START

"CHANGE3 START "NEED TO UPDATE THE RESOURCE_FLG
  IF LT_TARGET_ACCT IS NOT INITIAL AND ( REGION = 'ME' ) AND GATE5 = 1 .
    ""AND <LS_TIME> = CURRENT_PERIOD .  "CALC IS ONLY FOR INP_ME001

    CLEAR: FACTOR1 .

    READ TABLE LT_TARGET_ACCT INTO WA_MEMBER
    WITH KEY TABLE_LINE = 'RP900006'
    BINARY SEARCH.
    IF SY-SUBRC = 0.
      READ TABLE LT_RESOURCE_FLG
      INTO LS_RESOURCE
      WITH KEY
      AUDITTRAIL = ORIG_AUDITTRAIL
      CATEGORY   = ORIG_CATEGORY
      EMPLOYEE   = ORIG_EMPLOYEE
      ENTITY     = ORIG_ENTITY
      RESOURCE_ACCT  = 'RP900006'
      RPTCURRENCY    = ORIG_CURRENCY
      TIME           = 'XXXX.INP'
      BINARY SEARCH.
      IF SY-SUBRC = 0.
        FACTOR1 = LS_RESOURCE-SIGNEDDATA.
      ELSE.
        FACTOR1 = 0.
      ENDIF.

      CASE FACTOR1.

        WHEN 10 .
          ACCOUNT_DRIVER = 'RP000100'.
        WHEN 11 .
          ACCOUNT_DRIVER = 'RP000101'.
        WHEN 12 .
          ACCOUNT_DRIVER = 'RP000102'.
        WHEN 13 .
          ACCOUNT_DRIVER = 'RP000103'.
        WHEN 14 .
          ACCOUNT_DRIVER = 'RP000104'.
        WHEN 15 .
          ACCOUNT_DRIVER = 'RP000105'.
        WHEN 16 .
          ACCOUNT_DRIVER = 'RP000106'.
        WHEN 17 .
          ACCOUNT_DRIVER = 'RP000107'.
        WHEN OTHERS.
          ACCOUNT_DRIVER = 'DONOTHING'.
        ENDCASE.

        CLEAR LS_RESOURCE.
        READ TABLE LT_RESOURCE_FLG
        INTO LS_RESOURCE
        WITH KEY
        AUDITTRAIL      = ORIG_AUDITTRAIL
        CATEGORY        = ORIG_CATEGORY
        EMPLOYEE        = 'NO_EMPLOYEE'
        ENTITY          = PROFIT_CENTRE "ENTITY_DRIVER
        RESOURCE_ACCT   =  ACCOUNT_DRIVER
        RPTCURRENCY     = ORIG_CURRENCY
        TIME            = YEAR_INP
        BINARY SEARCH.
        IF SY-SUBRC = 0.
          <LS_SIGNEDDATA>     = LS_RESOURCE-SIGNEDDATA.
          <LS_RESOURCE_ACCT>  = 'RP010008' .
          <LS_TIME>           = YEAR_INP.
          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

        "BELOW UPDATING THE LT_RESOURCE_FLG START
        IF ACCOUNT_DRIVER <> 'DONOTHING'.
        CLEAR LS_RESOURCE.
        LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
        LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
        LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
        LS_RESOURCE-ENTITY      = <LS_ENTITY>.
        LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT> .
        LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
        LS_RESOURCE-TIME          = <LS_TIME>."YEAR_INP.
        LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
        LOOP AT LT_RESOURCE_FLG ASSIGNING <LS_RESOURCE> WHERE
          AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
          AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
          AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
          AND RPTCURRENCY = <LS_RPTCURRENCY> AND
          TIME = <LS_TIME>.
          <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
          ENDLOOP.
          IF SY-SUBRC <> 0.
            INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_FLG.
            ENDIF.
         ENDIF.
        "ABOVE UPDATING THE LT_RESOURCE_FLG END
    ENDIF.

    ENDIF.

    ENDIF.
"CHANGE3 END "NEED TO UPDATE THE RESOURCE_FLG


  IF ( RESOURCE_TYPE = 'E' AND EMPLOYEE_FLAG = 'Y' )  OR ( VAR_RP000425 = 0 AND ROLES_FLAG = 'Y' ). "CALC IS ONLY FOR PERM EMP OR PERM ROLES

    CLEAR: FACTOR1 , FACTOR2, FACTOR3, FACTOR4 , FACTOR5, FACTOR6, FACTOR7, FACTOR8,
           FACTOR9, VAR_NONPENSIONALL , FACTOR14 , FACTOR15, FACTOR16 .
    CLEAR LS_RESOURCE.

    LOOP AT LT_NONPENSIONALL INTO WA_MEMBER.
       FACTOR3 = FACTOR4.
       CLEAR LS_RESOURCE.
       READ TABLE LT_RESOURCE_FLG
       INTO LS_RESOURCE
       WITH KEY
       AUDITTRAIL    = ORIG_AUDITTRAIL
       CATEGORY      = ORIG_CATEGORY
       EMPLOYEE      = ORIG_EMPLOYEE
       ENTITY        = ORIG_ENTITY
       RESOURCE_ACCT  = WA_MEMBER
       RPTCURRENCY    = ORIG_CURRENCY
       TIME           = YEAR_INP
       BINARY SEARCH.
       IF SY-SUBRC = 0 .
         FACTOR4 =  LS_RESOURCE-SIGNEDDATA + FACTOR3.
         VAR_NONPENSIONALL = FACTOR4.
         ENDIF.
      ENDLOOP.
      CLEAR: FACTOR1 , FACTOR2, FACTOR3, FACTOR4 , FACTOR5, FACTOR6
      , FACTOR7.

       CLEAR LS_RESOURCE.
       READ TABLE LT_RESOURCE_FLG
       INTO LS_RESOURCE
       WITH KEY
       AUDITTRAIL    = ORIG_AUDITTRAIL
       CATEGORY      = ORIG_CATEGORY
       EMPLOYEE      = ORIG_EMPLOYEE
       ENTITY        = ORIG_ENTITY
       RESOURCE_ACCT  = 'RP010008' "FACTOR1
       RPTCURRENCY    = ORIG_CURRENCY
       TIME           = YEAR_INP
       BINARY SEARCH.
       IF SY-SUBRC = 0 .
         FACTOR1 =  LS_RESOURCE-SIGNEDDATA .
         ELSE.
         FACTOR1 = 0.
         ENDIF.

       CLEAR LS_RESOURCE.
       READ TABLE LT_RESOURCE_FLG
       INTO LS_RESOURCE
       WITH KEY
       AUDITTRAIL    = ORIG_AUDITTRAIL
       CATEGORY      = ORIG_CATEGORY
       EMPLOYEE      = ORIG_EMPLOYEE
       ENTITY        = ORIG_ENTITY
       RESOURCE_ACCT  = 'RP000020' "FACTOR2
       RPTCURRENCY    = ORIG_CURRENCY
       TIME           = YEAR_INP
       BINARY SEARCH.
       IF SY-SUBRC = 0 .
         FACTOR2 =  LS_RESOURCE-SIGNEDDATA .
         ELSE.
         FACTOR2 = 0.
         ENDIF.

       CLEAR LS_RESOURCE.
       READ TABLE LT_RESOURCE_FLG
       INTO LS_RESOURCE
       WITH KEY
       AUDITTRAIL    = ORIG_AUDITTRAIL
       CATEGORY      = ORIG_CATEGORY
       EMPLOYEE      = ORIG_EMPLOYEE
       ENTITY        = ORIG_ENTITY
       RESOURCE_ACCT  = 'RP000026' "FACTOR3
       RPTCURRENCY    = ORIG_CURRENCY
       TIME           = YEAR_INP
       BINARY SEARCH.
       IF SY-SUBRC = 0 .
         FACTOR3 =  LS_RESOURCE-SIGNEDDATA .
         ELSE.
         FACTOR3 = 0.
         ENDIF.

       CLEAR LS_RESOURCE.
       READ TABLE LT_RESOURCE_FLG
       INTO LS_RESOURCE
       WITH KEY
       AUDITTRAIL    = ORIG_AUDITTRAIL
       CATEGORY      = ORIG_CATEGORY
       EMPLOYEE      = ORIG_EMPLOYEE
       ENTITY        = ORIG_ENTITY
       RESOURCE_ACCT  = 'RP000021' "FACTOR4
       RPTCURRENCY    = ORIG_CURRENCY
       TIME           = YEAR_INP
       BINARY SEARCH.
       IF SY-SUBRC = 0 .
         FACTOR4 =  LS_RESOURCE-SIGNEDDATA .
         ELSE.
         FACTOR4 = 0.
         ENDIF.

       CLEAR LS_RESOURCE.
       READ TABLE LT_RESOURCE_FLG
       INTO LS_RESOURCE
       WITH KEY
       AUDITTRAIL    = ORIG_AUDITTRAIL
       CATEGORY      = ORIG_CATEGORY
       EMPLOYEE      = ORIG_EMPLOYEE
       ENTITY        = ORIG_ENTITY
       RESOURCE_ACCT  = 'RP010019' "FACTOR5
       RPTCURRENCY    = ORIG_CURRENCY
       TIME           = YEAR_INP
       BINARY SEARCH.
       IF SY-SUBRC = 0 .
         FACTOR5 =  LS_RESOURCE-SIGNEDDATA .
         ELSE.
         FACTOR5 = 0.
         ENDIF.

       CLEAR LS_RESOURCE.
       READ TABLE LT_RESOURCE_FLG
       INTO LS_RESOURCE
       WITH KEY
       AUDITTRAIL    = ORIG_AUDITTRAIL
       CATEGORY      = ORIG_CATEGORY
       EMPLOYEE      = ORIG_EMPLOYEE
       ENTITY        = ORIG_ENTITY
       RESOURCE_ACCT  = 'RP010020' "FACTOR6
       RPTCURRENCY    = ORIG_CURRENCY
       TIME           = YEAR_INP
       BINARY SEARCH.
       IF SY-SUBRC = 0 .
         FACTOR6 =  LS_RESOURCE-SIGNEDDATA .
         ELSE.
         FACTOR6 = 0.
         ENDIF.

       CLEAR LS_RESOURCE.
       READ TABLE LT_RESOURCE_FLG
       INTO LS_RESOURCE
       WITH KEY
       AUDITTRAIL    = ORIG_AUDITTRAIL
       CATEGORY      = ORIG_CATEGORY
       EMPLOYEE      = ORIG_EMPLOYEE
       ENTITY        = ORIG_ENTITY
       RESOURCE_ACCT  = 'RP010021' "FACTOR7
       RPTCURRENCY    = ORIG_CURRENCY
       TIME           = YEAR_INP
       BINARY SEARCH.
       IF SY-SUBRC = 0 .
         FACTOR7 =  LS_RESOURCE-SIGNEDDATA .
         ELSE.
         FACTOR7 = 0.
         ENDIF.

       CLEAR LS_RESOURCE.
       READ TABLE LT_RESOURCE_FLG
       INTO LS_RESOURCE
       WITH KEY
       AUDITTRAIL    = ORIG_AUDITTRAIL
       CATEGORY      = ORIG_CATEGORY
       EMPLOYEE      = ORIG_EMPLOYEE
       ENTITY        = ORIG_ENTITY
       RESOURCE_ACCT  = 'RP010022' "FACTOR8
       RPTCURRENCY    = ORIG_CURRENCY
       TIME           = YEAR_INP
       BINARY SEARCH.
       IF SY-SUBRC = 0 .
         FACTOR8 =  LS_RESOURCE-SIGNEDDATA .
         ELSE.
         FACTOR8 = 0.
         ENDIF.

       CLEAR LS_RESOURCE.
       READ TABLE LT_RESOURCE_FLG
       INTO LS_RESOURCE
       WITH KEY
       AUDITTRAIL    = ORIG_AUDITTRAIL
       CATEGORY      = ORIG_CATEGORY
       EMPLOYEE      = ORIG_EMPLOYEE
       ENTITY        = ORIG_ENTITY
       RESOURCE_ACCT  = 'RP010023' "FACTOR9
       RPTCURRENCY    = ORIG_CURRENCY
       TIME           = YEAR_INP
       BINARY SEARCH.
       IF SY-SUBRC = 0 .
         FACTOR9 =  LS_RESOURCE-SIGNEDDATA .
         ELSE.
         FACTOR9 = 0.
         ENDIF.

       CLEAR LS_RESOURCE.
       READ TABLE LT_RESOURCE_FLG
       INTO LS_RESOURCE
       WITH KEY
       AUDITTRAIL    = ORIG_AUDITTRAIL
       CATEGORY      = ORIG_CATEGORY
       EMPLOYEE      = ORIG_EMPLOYEE
       ENTITY        = ORIG_ENTITY
       RESOURCE_ACCT  = 'RP010024' "FACTOR10
       RPTCURRENCY    = ORIG_CURRENCY
       TIME           = YEAR_INP
       BINARY SEARCH.
       IF SY-SUBRC = 0 .
         FACTOR10 =  LS_RESOURCE-SIGNEDDATA .
         ELSE.
         FACTOR10 = 0.
         ENDIF.

       CLEAR LS_RESOURCE.
       READ TABLE LT_RESOURCE_FLG
       INTO LS_RESOURCE
       WITH KEY
       AUDITTRAIL    = ORIG_AUDITTRAIL
       CATEGORY      = ORIG_CATEGORY
       EMPLOYEE      = ORIG_EMPLOYEE
       ENTITY        = ORIG_ENTITY
       RESOURCE_ACCT  = 'RP010025' "FACTOR11
       RPTCURRENCY    = ORIG_CURRENCY
       TIME           = YEAR_INP
       BINARY SEARCH.
       IF SY-SUBRC = 0 .
         FACTOR11 =  LS_RESOURCE-SIGNEDDATA .
         ELSE.
         FACTOR11 = 0.
         ENDIF.

       CLEAR LS_RESOURCE.
       READ TABLE LT_RESOURCE_FLG
       INTO LS_RESOURCE
       WITH KEY
       AUDITTRAIL    = ORIG_AUDITTRAIL
       CATEGORY      = ORIG_CATEGORY
       EMPLOYEE      = ORIG_EMPLOYEE
       ENTITY        = ORIG_ENTITY
       RESOURCE_ACCT  = 'RP010026' "FACTOR12
       RPTCURRENCY    = ORIG_CURRENCY
       TIME           = YEAR_INP
       BINARY SEARCH.
       IF SY-SUBRC = 0 .
         FACTOR12 =  LS_RESOURCE-SIGNEDDATA .
         ELSE.
         FACTOR12 = 0.
         ENDIF.

       CLEAR LS_RESOURCE.
       READ TABLE LT_RESOURCE_FLG
       INTO LS_RESOURCE
       WITH KEY
       AUDITTRAIL    = ORIG_AUDITTRAIL
       CATEGORY      = ORIG_CATEGORY
       EMPLOYEE      = ORIG_EMPLOYEE
       ENTITY        = ORIG_ENTITY
       RESOURCE_ACCT  = 'RP000027'
       RPTCURRENCY    = ORIG_CURRENCY
       TIME           = 'XXXX.INP'
       BINARY SEARCH.
       IF SY-SUBRC = 0 .
         FACTOR20 =  LS_RESOURCE-SIGNEDDATA .
         ELSE.
         FACTOR20 = 0.
         ENDIF.

*****
    LOOP AT LT_TIMESCOPE INTO LS_TIME. "GEN CALC 5MAIN LOOP
       CLEAR LS_RESOURCE.
       READ TABLE LT_RESOURCE_TX
       INTO LS_RESOURCE
       WITH KEY
       AUDITTRAIL    = ORIG_AUDITTRAIL
       CATEGORY      = ORIG_CATEGORY
       EMPLOYEE      = ORIG_EMPLOYEE
       ENTITY        = ORIG_ENTITY
       RESOURCE_ACCT  = 'RP000408' "FTE VALUE
       RPTCURRENCY    = ORIG_CURRENCY
       TIME           = LS_TIME-ID
       BINARY SEARCH.
       IF SY-SUBRC = 0 .
         FACTOR13 = LS_RESOURCE-SIGNEDDATA .
         ELSE.
           FACTOR13 = 0.
           ENDIF.

      "DO A ROUND UP OF FTE
           "ROUND UP FACTOR14 TO NEAREST WHOLE NUMBER I.E. .25 => 1 ; 1.2 => 2
      IF FACTOR13 > 0.
        "GET THE WHOLE NUMBER PART AND DECIMAL PART
        FACTOR14 = FACTOR13 MOD 1.
        FACTOR15 = FACTOR13 DIV 1.

        IF FACTOR14 > 0.
          FACTOR14 = 1.
          ELSE.
            FACTOR14 = 0.
            ENDIF.
        FACTOR14 = FACTOR15 + FACTOR14.


       ELSEIF FACTOR13 < 0.
        "GET THE WHOLE NUMBER PART AND DECIMAL PART
        FACTOR13 = FACTOR13 * -1 .
        FACTOR14 = FACTOR13 MOD 1.
        FACTOR15 = FACTOR13 DIV 1.

        IF FACTOR14 > 0.
          FACTOR14 = 1.
          ELSE.
            FACTOR14 = 0.
            ENDIF.
        FACTOR14 = FACTOR15 + FACTOR14.

        FACTOR14 = FACTOR14 * -1 .
        FACTOR13 = FACTOR13 * -1 .

        ENDIF.

      "// Calculate monthly pensionable allowances x FTE END

        <LS_SIGNEDDATA> = (  VAR_PENSIONALLOW / 12 ) *  FACTOR13.
        <LS_RESOURCE_ACCT> = 'RP020004' .
        <LS_TIME> = LS_TIME-ID .
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
       "BELOW UPDATING THE LT_RESOURCE_TX START
        CLEAR LS_RESOURCE.
        LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
        LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
        LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
        LS_RESOURCE-ENTITY      = <LS_ENTITY>.
        LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
        LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
        LS_RESOURCE-TIME          = <LS_TIME>.
        LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
        LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
          AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
          AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
          AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
          AND RPTCURRENCY = <LS_RPTCURRENCY> AND
          TIME = <LS_TIME>.
          <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
          ENDLOOP.
          IF SY-SUBRC <> 0.
            INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
            ENDIF.
        "ABOVE UPDATING THE LT_RESOURCE_TX END

      "// Calculate total allowances
        <LS_SIGNEDDATA> = ( ( VAR_PENSIONALLOW + VAR_NONPENSIONALL ) / 12 ) *  FACTOR13.
        <LS_RESOURCE_ACCT> = 'AC710060' .
        <LS_TIME> = LS_TIME-ID .
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

        IF FACTOR13 <> 0 . "LS_RESOURCE-SIGNEDDATA <> 0."FTE NOT ZERO OR BLANK
"// Calculate monthly medical cost - cost is not FTE dependent
"Take RP010008 in TIME = YEAR_INP => FACTOR1
        <LS_SIGNEDDATA> = ( (  FACTOR1 ) / 12 ) * FACTOR14.
        <LS_RESOURCE_ACCT> = 'RP020005' .
        <LS_TIME> = LS_TIME-ID .
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            "BELOW UPDATING THE LT_RESOURCE_TX START
            CLEAR LS_RESOURCE.
            LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
            LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
            LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
            LS_RESOURCE-ENTITY      = <LS_ENTITY>.
            LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
            LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
            LS_RESOURCE-TIME          = <LS_TIME>.
            LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
            LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
              AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
              AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
              AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
              AND RPTCURRENCY = <LS_RPTCURRENCY> AND
              TIME = <LS_TIME>.
              <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
              ENDLOOP.
              IF SY-SUBRC <> 0.
                INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
                ENDIF.
            "ABOVE UPDATING THE LT_RESOURCE_TX END
        <LS_SIGNEDDATA> = ( (  FACTOR1 ) / 12 ) * FACTOR14.
        <LS_RESOURCE_ACCT> = 'AC710310' .
        <LS_TIME> = LS_TIME-ID .
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
        IF FACTOR2 <> 0 AND FACTOR20 <> 0. "IF FACTOR2 <> 0 SINCE WE CAN ONLY HAVE RP000020 OR RP000021

"// Calculate monthly car allowance - cost is not FTE dependent
"Take RP000020 in TIME = YEAR_INP
        <LS_SIGNEDDATA> = (  FACTOR2 + FACTOR3 ) / 12  * FACTOR14.
        <LS_RESOURCE_ACCT> = 'RP020006' .
        <LS_TIME> = LS_TIME-ID .
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            "BELOW UPDATING THE LT_RESOURCE_TX START
            CLEAR LS_RESOURCE.
            LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
            LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
            LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
            LS_RESOURCE-ENTITY      = <LS_ENTITY>.
            LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
            LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
            LS_RESOURCE-TIME          = <LS_TIME>.
            LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
            LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
              AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
              AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
              AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
              AND RPTCURRENCY = <LS_RPTCURRENCY> AND
              TIME = <LS_TIME>.
              <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
              ENDLOOP.
              IF SY-SUBRC <> 0.
                INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
                ENDIF.
            "ABOVE UPDATING THE LT_RESOURCE_TX END
        <LS_SIGNEDDATA> = (  FACTOR2 + FACTOR3 ) / 12 * FACTOR14 .
        <LS_RESOURCE_ACCT> = 'AC740000' .
        <LS_TIME> = LS_TIME-ID .
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
          "BELOW UPDATING THE LT_RESOURCE_TX START
          CLEAR LS_RESOURCE.
          LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
          LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
          LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
          LS_RESOURCE-ENTITY      = <LS_ENTITY>.
          LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
          LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
          LS_RESOURCE-TIME          = <LS_TIME>.
          LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
          LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
            AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
            AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
            AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
            AND RPTCURRENCY = <LS_RPTCURRENCY> AND
            TIME = <LS_TIME>.
            <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
            ENDLOOP.
            IF SY-SUBRC <> 0.
              INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
              ENDIF.
          "ABOVE UPDATING THE LT_RESOURCE_TX END
              ELSEIF FACTOR4 <> 0 AND FACTOR20 <> 0. "IF FACTOR2 <> 0 SINCE WE CAN ONLY HAVE RP000020 OR RP000021
"// Calculate monthly car cash equivalent - cost is not FTE dependent
"Take RP000021 in TIME = YEAR_INP => FACTOR4
        <LS_SIGNEDDATA> = (  FACTOR4 ) / 12 * FACTOR14  .
        <LS_RESOURCE_ACCT> = 'RP020006' .
        <LS_TIME> = LS_TIME-ID .
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            "BELOW UPDATING THE LT_RESOURCE_TX START
            CLEAR LS_RESOURCE.
            LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
            LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
            LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
            LS_RESOURCE-ENTITY      = <LS_ENTITY>.
            LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
            LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
            LS_RESOURCE-TIME          = <LS_TIME>.
            LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
            LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
              AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
              AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
              AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
              AND RPTCURRENCY = <LS_RPTCURRENCY> AND
              TIME = <LS_TIME>.
              <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
              ENDLOOP.
              IF SY-SUBRC <> 0.
                INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
                ENDIF.
            "ABOVE UPDATING THE LT_RESOURCE_TX END
        <LS_SIGNEDDATA> = (  FACTOR4 ) / 12 * FACTOR14  .
        <LS_RESOURCE_ACCT> = 'AC740120'. "'AC710310' . "THIS IS NOT THE CORRECT ACCT GET FROM CG!!!
        <LS_TIME> = LS_TIME-ID .
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
        ENDIF."IF FACTOR2 <> 0 SINCE WE CAN ONLY HAVE RP000020 OR RP000021
"// Calculate school fees
"Take RP010019  in TIME = YEAR_INP => FACTOR5
        <LS_SIGNEDDATA> = (  FACTOR5 ) / 12  .
        <LS_RESOURCE_ACCT> = 'AC710280' .
        <LS_TIME> = LS_TIME-ID .
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

"// Calculate housing & notional housing
"Calculate (RP010020 + RP010021) in TIME = YEAR_INP FACTOR6 + FACTOR7
        <LS_SIGNEDDATA> = (  FACTOR6 + FACTOR7 ) / 12  .
        <LS_RESOURCE_ACCT> = 'AC710291' .
        <LS_TIME> = LS_TIME-ID .
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

"// Calculate fuel
"Take RP010023  in TIME = YEAR_INP => FACTOR9
        <LS_SIGNEDDATA> = (  FACTOR9 ) / 12  .
        <LS_RESOURCE_ACCT> = 'AC750045' . "'AC800040' .
        <LS_TIME> = LS_TIME-ID .
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

"// Calculate work permit
"Calculate (RP010024 + RP010025  in TIME = YEAR_INP
        <LS_SIGNEDDATA> = (  FACTOR10 + FACTOR11 ) / 12  .
        <LS_RESOURCE_ACCT> = 'AC710270' .
        <LS_TIME> = LS_TIME-ID .
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

"// MEDICAL FOR VISA
"Take RP010026  in TIME = YEAR_INP FACTOR12
        <LS_SIGNEDDATA> = (  FACTOR12 ) / 12  .
        <LS_RESOURCE_ACCT> = 'AC780040' .
        <LS_TIME> = LS_TIME-ID .
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

         ELSEIF FACTOR13 = 0. "FTE IS ZERO POST ZERO TO RP020005 - 6
          <LS_SIGNEDDATA> = 0 .
          <LS_RESOURCE_ACCT> = 'RP020005' .
          <LS_TIME> = LS_TIME-ID .
          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
              "BELOW UPDATING THE LT_RESOURCE_TX START
              CLEAR LS_RESOURCE.
              LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
              LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
              LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
              LS_RESOURCE-ENTITY      = <LS_ENTITY>.
              LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
              LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
              LS_RESOURCE-TIME          = <LS_TIME>.
              LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
              LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
                AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
                AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
                AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
                AND RPTCURRENCY = <LS_RPTCURRENCY> AND
                TIME = <LS_TIME>.
                <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
                ENDLOOP.
                IF SY-SUBRC <> 0.
                  INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
                  ENDIF.
              "ABOVE UPDATING THE LT_RESOURCE_TX END
            <LS_SIGNEDDATA> = 0 .
            <LS_RESOURCE_ACCT> = 'RP020006' .
            <LS_TIME> = LS_TIME-ID .
            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
                "BELOW UPDATING THE LT_RESOURCE_TX START
                CLEAR LS_RESOURCE.
                LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
                LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
                LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
                LS_RESOURCE-ENTITY      = <LS_ENTITY>.
                LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
                LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
                LS_RESOURCE-TIME          = <LS_TIME>.
                LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
                LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
                  AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
                  AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
                  AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
                  AND RPTCURRENCY = <LS_RPTCURRENCY> AND
                  TIME = <LS_TIME>.
                  <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
                  ENDLOOP.
                  IF SY-SUBRC <> 0.
                    INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
                    ENDIF.
                "ABOVE UPDATING THE LT_RESOURCE_TX END

         ENDIF."FTE NOT ZERO OR BLANK
      ENDLOOP."GEN CALC 5MAIN LOOP

   ENDIF.
"GEN CALC 5 CALCULATE OTHER MONTHLY VALUES END
*********************************************************************************************
"UK SPECIFIC CALC START

IF REGION = 'UK'." UK REGION

  CLEAR: FACTOR1 , FACTOR2, FACTOR3, FACTOR4 , FACTOR5, FACTOR6, FACTOR7,
  FACTOR8, FACTOR9 , FACTOR10 ,FACTOR11, FACTOR12,FACTOR16,FACTOR14,FACTOR13,FACTOR15, TIME_STRING.
  CLEAR: FACTOR17, FACTOR18, FACTOR19 . "CR3102
  CLEAR LS_RESOURCE.
"// Calculate SMART salary sacrifice deduction
"//Basic Rate Tax Lookup %BASIC_RATE%

  CLEAR LS_RESOURCE.
  READ TABLE LT_RESOURCE_FLG
  INTO LS_RESOURCE
  WITH KEY
  AUDITTRAIL    = ORIG_AUDITTRAIL
  CATEGORY      = ORIG_CATEGORY
  EMPLOYEE      = 'NO_EMPLOYEE'
  ENTITY        = 'NO_PROFITCENTREUK'
  RESOURCE_ACCT  = 'RP000025' "FACTOR1 BASIC TAX RATE
  RPTCURRENCY    = ORIG_CURRENCY
  TIME           = YEAR_INP
  BINARY SEARCH.
  IF SY-SUBRC = 0 .
    FACTOR1 =  LS_RESOURCE-SIGNEDDATA .
    ELSE.
    FACTOR1 = 0.
    ENDIF.

  CLEAR LS_RESOURCE.
  READ TABLE LT_RESOURCE_FLG
  INTO LS_RESOURCE
  WITH KEY
  AUDITTRAIL    = ORIG_AUDITTRAIL
  CATEGORY      = ORIG_CATEGORY
  EMPLOYEE      = ORIG_EMPLOYEE
  ENTITY        = ORIG_ENTITY
  RESOURCE_ACCT  = 'RP000005' "FACTOR2
  RPTCURRENCY    = ORIG_CURRENCY
  TIME           = 'XXXX.INP'
  BINARY SEARCH.
  IF SY-SUBRC = 0 .
    FACTOR2 =  LS_RESOURCE-SIGNEDDATA .
    ELSE.
    FACTOR2 = 0.
    ENDIF.

  CLEAR LS_RESOURCE.
  READ TABLE LT_RESOURCE_FLG
  INTO LS_RESOURCE
  WITH KEY
  AUDITTRAIL    = ORIG_AUDITTRAIL
  CATEGORY      = ORIG_CATEGORY
  EMPLOYEE      = ORIG_EMPLOYEE
  ENTITY        = ORIG_ENTITY
  RESOURCE_ACCT  = 'RP000009' "FACTOR3
  RPTCURRENCY    = ORIG_CURRENCY
  TIME           = 'XXXX.INP'
  BINARY SEARCH.
  IF SY-SUBRC = 0 .
    FACTOR3 =  LS_RESOURCE-SIGNEDDATA .
    ELSE.
    FACTOR3 = 0.
    ENDIF.

  CLEAR LS_RESOURCE.
  READ TABLE LT_RESOURCE_FLG
  INTO LS_RESOURCE
  WITH KEY
  AUDITTRAIL    = ORIG_AUDITTRAIL
  CATEGORY      = ORIG_CATEGORY
  EMPLOYEE      = ORIG_EMPLOYEE
  ENTITY        = ORIG_ENTITY
  RESOURCE_ACCT  = 'RP000405' "FACTOR4
  RPTCURRENCY    = ORIG_CURRENCY
  TIME           = 'XXXX.INP'
  BINARY SEARCH.
  IF SY-SUBRC = 0 .
    FACTOR4 =  LS_RESOURCE-SIGNEDDATA .
    ELSE.
    FACTOR4 = 0.
    ENDIF.

  CLEAR LS_RESOURCE.
  READ TABLE LT_RESOURCE_FLG
  INTO LS_RESOURCE
  WITH KEY
  AUDITTRAIL    = ORIG_AUDITTRAIL
  CATEGORY      = ORIG_CATEGORY
  EMPLOYEE      = ORIG_EMPLOYEE
  ENTITY        = ORIG_ENTITY
  RESOURCE_ACCT  = 'RP000425' "FACTOR5
  RPTCURRENCY    = ORIG_CURRENCY
  TIME           = 'XXXX.INP'
  BINARY SEARCH.
  IF SY-SUBRC = 0 .
    FACTOR5 =  LS_RESOURCE-SIGNEDDATA .
    ELSE.
    FACTOR5 = 0.
    ENDIF.

  CLEAR LS_RESOURCE.
  READ TABLE LT_RESOURCE_FLG
  INTO LS_RESOURCE
  WITH KEY
  AUDITTRAIL    = ORIG_AUDITTRAIL
  CATEGORY      = ORIG_CATEGORY
  EMPLOYEE      = ORIG_EMPLOYEE
  ENTITY        = ORIG_ENTITY
  RESOURCE_ACCT  = 'RP000406' "FACTOR12
  RPTCURRENCY    = ORIG_CURRENCY
  TIME           = 'XXXX.INP'
  BINARY SEARCH.
  IF SY-SUBRC = 0 .
    FACTOR12 =  LS_RESOURCE-SIGNEDDATA .
    ELSE.
    FACTOR12 = 0.
    ENDIF.

  CLEAR LS_RESOURCE.
  READ TABLE LT_RESOURCE_FLG
  INTO LS_RESOURCE
  WITH KEY
  AUDITTRAIL    = ORIG_AUDITTRAIL
  CATEGORY      = ORIG_CATEGORY
  EMPLOYEE      = ORIG_EMPLOYEE
  ENTITY        = ORIG_ENTITY
  RESOURCE_ACCT  = 'RP900004' "FACTOR13
  RPTCURRENCY    = ORIG_CURRENCY
  TIME           = 'XXXX.INP'
  BINARY SEARCH.
  IF SY-SUBRC = 0 .
    FACTOR13 =  LS_RESOURCE-SIGNEDDATA .
    ELSE.
    FACTOR13 = 0.
    ENDIF.

  LOOP AT LT_TIMESCOPE INTO LS_TIME."TIMESCOPE UK REGION START
         "GET TOTAL_PENSIONSALARY
      CLEAR: FACTOR6 , FACTOR7, VAR_PENSIONSALARY .
      LOOP AT LT_PENSIONSALARY INTO WA_MEMBER.
       FACTOR6 = FACTOR7.
       CLEAR LS_RESOURCE.
       READ TABLE LT_RESOURCE_TX
       INTO LS_RESOURCE
       WITH KEY
       AUDITTRAIL    = ORIG_AUDITTRAIL
       CATEGORY      = ORIG_CATEGORY
       EMPLOYEE      = ORIG_EMPLOYEE
       ENTITY        = ORIG_ENTITY
       RESOURCE_ACCT  = WA_MEMBER
       RPTCURRENCY    = ORIG_CURRENCY
       TIME           = LS_TIME-ID
       BINARY SEARCH.
       IF SY-SUBRC = 0 .
         FACTOR7 =  LS_RESOURCE-SIGNEDDATA + FACTOR6.
         VAR_PENSIONSALARY = FACTOR7.
          ENDIF.
      ENDLOOP.

        "GET 'TOTAL_NIABLESALARY'
      CLEAR: FACTOR6 , FACTOR7 ,VAR_NIABLESALARY.
      LOOP AT LT_NIABLESALARY INTO WA_MEMBER.
       FACTOR6 = FACTOR7.
       CLEAR LS_RESOURCE.
       READ TABLE LT_RESOURCE_TX
       INTO LS_RESOURCE
       WITH KEY
       AUDITTRAIL    = ORIG_AUDITTRAIL
       CATEGORY      = ORIG_CATEGORY
       EMPLOYEE      = ORIG_EMPLOYEE
       ENTITY        = ORIG_ENTITY
       RESOURCE_ACCT  = WA_MEMBER
       RPTCURRENCY    = ORIG_CURRENCY
       TIME           = LS_TIME-ID
       BINARY SEARCH.
       IF SY-SUBRC = 0 .
         FACTOR7 =  LS_RESOURCE-SIGNEDDATA + FACTOR6.
         VAR_NIABLESALARY = FACTOR7.
          ENDIF.
      ENDLOOP.
    CLEAR: FACTOR6 , FACTOR7 .
"UK CALC 2 SMART DEDUCTION
    IF ( RESOURCE_TYPE = 'E' AND EMPLOYEE_FLAG = 'Y' )  OR ( VAR_RP000425 = 0 AND ROLES_FLAG = 'Y' ). "CALC IS ONLY FOR PERM EMP OR PERM ROLES

        READ TABLE LT_TOTAL_ROLES TRANSPORTING NO FIELDS
        WITH KEY TABLE_LINE = ORIG_EMPLOYEE.
        IF SY-SUBRC = 0 AND FACTOR5 = 0.
          "When Employee = BAS(TOTAL_ROLES) and RP000425 TIME = CATEGORY.YEAR&".INP" = 0 or [BLANK]
            IF FACTOR2 = 1 AND FACTOR3 = 1.
              <LS_SIGNEDDATA> = VAR_PENSIONSALARY * FACTOR4 * ( 1 + FACTOR1 ).
              FACTOR6 = <LS_SIGNEDDATA> .
              <LS_TIME> = LS_TIME-ID .
              <LS_RESOURCE_ACCT> = 'AC710005' .
              <LS_SIGNEDDATA> = <LS_SIGNEDDATA> * -1. "CR NEW_002
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
              <LS_SIGNEDDATA> = FACTOR6 . "CR NEW_002
              <LS_RESOURCE_ACCT> = 'RP020007' .
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

              ELSEIF FACTOR2 = 1 AND FACTOR3 = 0.
              <LS_SIGNEDDATA> = VAR_PENSIONSALARY * FACTOR4 .
              FACTOR6 = <LS_SIGNEDDATA> .
              <LS_RESOURCE_ACCT> = 'AC710005' .
              <LS_TIME> = LS_TIME-ID .
              <LS_SIGNEDDATA> = <LS_SIGNEDDATA> * -1. "CR NEW_002
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
              <LS_SIGNEDDATA> = FACTOR6 . "CR NEW_002
              <LS_RESOURCE_ACCT> = 'RP020007' .
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

              ELSE. "DID NOT FIT INTO ANY FACTOR ABOVE
              <LS_SIGNEDDATA> = 0 .
              FACTOR6 = <LS_SIGNEDDATA> .
              <LS_RESOURCE_ACCT> = 'AC710005' .
              <LS_TIME> = LS_TIME-ID .
              <LS_SIGNEDDATA> = <LS_SIGNEDDATA> * -1. "CR NEW_002
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
              <LS_SIGNEDDATA> = FACTOR6 . "CR NEW_002
              <LS_RESOURCE_ACCT> = 'RP020007' .
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

              ENDIF.

              "BELOW UPDATING THE LT_RESOURCE_TX START
              CLEAR LS_RESOURCE.
              LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
              LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
              LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
              LS_RESOURCE-ENTITY      = <LS_ENTITY>.
              LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
              LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
              LS_RESOURCE-TIME          = <LS_TIME>.
              LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
              LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
                AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
                AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
                AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
                AND RPTCURRENCY = <LS_RPTCURRENCY> AND
                TIME = <LS_TIME>.
                <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
                ENDLOOP.
                IF SY-SUBRC <> 0.
                  INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
                  ENDIF.
              "ABOVE UPDATING THE LT_RESOURCE_TX END

          ELSEIF RESOURCE_TYPE = 'E' AND EMPLOYEE_FLAG = 'Y'. "HENCE NOT IN BAS(TOTAL_ROLES) "BUT PERMANENT EMPLOYEE
          "When Employee = %PERM_EMP%
            IF FACTOR2 = 1 AND FACTOR3 = 1.
              <LS_SIGNEDDATA> = VAR_PENSIONSALARY * FACTOR4 * ( 1 + FACTOR1 ).
              FACTOR6 = <LS_SIGNEDDATA> .
              <LS_RESOURCE_ACCT> = 'AC710005' .
              <LS_TIME> = LS_TIME-ID .
              <LS_SIGNEDDATA> = <LS_SIGNEDDATA> * -1. "CR NEW_002
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
              <LS_SIGNEDDATA> = FACTOR6 . "CR NEW_002
              <LS_RESOURCE_ACCT> = 'RP020007' .
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

              ELSEIF FACTOR2 = 1 AND FACTOR3 = 0.
              <LS_SIGNEDDATA> = VAR_PENSIONSALARY * FACTOR4 .
              FACTOR6 = <LS_SIGNEDDATA> .
              <LS_RESOURCE_ACCT> = 'AC710005' .
              <LS_TIME> = LS_TIME-ID .
              <LS_SIGNEDDATA> = <LS_SIGNEDDATA> * -1. "CR NEW_002
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
              <LS_SIGNEDDATA> = FACTOR6 . "CR NEW_002
              <LS_RESOURCE_ACCT> = 'RP020007' .
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

              ELSE. "DID NOT FIT INTO ANY FACTOR ABOVE
              <LS_SIGNEDDATA> = 0 .
              FACTOR6 = <LS_SIGNEDDATA> .
              <LS_RESOURCE_ACCT> = 'AC710005' .
              <LS_TIME> = LS_TIME-ID .
              <LS_SIGNEDDATA> = <LS_SIGNEDDATA> * -1. "CR NEW_002
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
              <LS_SIGNEDDATA> = FACTOR6 . "CR NEW_002
              <LS_RESOURCE_ACCT> = 'RP020007' .
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

              ENDIF.
              "BELOW UPDATING THE LT_RESOURCE_TX START
              CLEAR LS_RESOURCE.
              LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
              LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
              LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
              LS_RESOURCE-ENTITY      = <LS_ENTITY>.
              LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
              LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
              LS_RESOURCE-TIME          = <LS_TIME>.
              LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
              LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
                AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
                AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
                AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
                AND RPTCURRENCY = <LS_RPTCURRENCY> AND
                TIME = <LS_TIME>.
                <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
                ENDLOOP.
                IF SY-SUBRC <> 0.
                  INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
                  ENDIF.
              "ABOVE UPDATING THE LT_RESOURCE_TX END

            ENDIF.
***        ENDCASE.
"UK CALC 3 - Calc Salary minus SMART

"// Calculate Total salary package - SMART deduction
          <LS_SIGNEDDATA> = VAR_NIABLESALARY - FACTOR6 .
           <LS_RESOURCE_ACCT> = 'RP020008' .
           FACTOR7 = <LS_SIGNEDDATA> .
           <LS_TIME> = LS_TIME-ID .
           COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

"UK CALC 4 - Calc Salary between lower and upper NI limit
        READ TABLE LT_TOTAL_ROLES TRANSPORTING NO FIELDS
        WITH KEY TABLE_LINE = ORIG_EMPLOYEE.
        IF SY-SUBRC = 0 AND FACTOR5 = 0.  "FACTOR5 = 1. as per CG
          "When Employee = BAS(TOTAL_ROLES) and RP000425 TIME = CATEGORY.YEAR&".INP" = 1
***"WHERE EMPLOYEE = BAS(TOTAL_ROLES) and RP000425 in TIME = CATEGORY.YEAR&"".INP"" = 1
***// Value between SP-UAP
***Calculate MAX(MIN(RP020008, (%UPPER_LIM% /12)),0- (%LOWER_LIM% /12),0) and write result in RP020009 for TIMESCOPE
          FACTOR8 = ( LOWER_LIM / 12 ). "* -1 .
          FACTOR9 = UPPER_LIM / 12 .
          FACTOR15 = FACTOR7 - FACTOR8 .

          "NEW FORMULA AS PER V1.0
          IF FACTOR7 < FACTOR8.
            <LS_SIGNEDDATA> = 0.
            ELSE.
              FACTOR10 = FACTOR9 - FACTOR8 .
              <LS_SIGNEDDATA> = NMIN( VAL1 = FACTOR15 VAL2 = FACTOR10 ).
              ENDIF.

           <LS_RESOURCE_ACCT> = 'RP020009' .
           <LS_TIME> = LS_TIME-ID .
           COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
           FACTOR11 = <LS_SIGNEDDATA>.

***// Value Over UAP
***Calculate MAX(RP020008 - (%UPPER_LIM% /12),0) and write into RP020010 for TIMESCOPE
          FACTOR7 = FACTOR7 - FACTOR9 .
          <LS_SIGNEDDATA> = NMAX( VAL1 = FACTOR7 VAL2 = 0 ) .
          <LS_RESOURCE_ACCT> = 'RP020010' .
           <LS_TIME> = LS_TIME-ID .
           COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
           FACTOR11 = FACTOR11 + <LS_SIGNEDDATA>.

          ELSEIF RESOURCE_TYPE = 'E' AND EMPLOYEE_FLAG = 'Y'. "HENCE NOT IN BAS(TOTAL_ROLES) "BUT PERMANENT EMPLOYEE

***// Value between SP-UAP
***Calculate MAX(MIN(RP020008, (%UPPER_LIM% /12)),0- (%LOWER_LIM% /12),0) and write result in RP020009 for TIMESCOPE
          FACTOR8 = ( LOWER_LIM / 12 ). "* -1 .
          FACTOR9 = UPPER_LIM / 12 .
          FACTOR15 = FACTOR7 - FACTOR8 .

          "NEW FORMULA AS PER V1.0
          IF FACTOR7 < FACTOR8.
            <LS_SIGNEDDATA> = 0.
            ELSE.
              FACTOR10 = FACTOR9 - FACTOR8 .
              <LS_SIGNEDDATA> = NMIN( VAL1 = FACTOR15 VAL2 = FACTOR10 ).
              ENDIF.

           <LS_RESOURCE_ACCT> = 'RP020009' .
           <LS_TIME> = LS_TIME-ID .
           COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
           FACTOR11 = <LS_SIGNEDDATA>.

***// Value Over UAP
***Calculate MAX(RP020008 - (%UPPER_LIM% /12), 0) and write into RP020010 for TIMESCOPE"
          FACTOR7 = FACTOR7 - FACTOR9 .
          <LS_SIGNEDDATA> = NMAX( VAL1 = FACTOR7 VAL2 = 0 ) .
          <LS_RESOURCE_ACCT> = 'RP020010' .
           <LS_TIME> = LS_TIME-ID .
           COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
           FACTOR11 = FACTOR11 + <LS_SIGNEDDATA>.

            ENDIF.

"Calculate (RP020009 + RP020010) for TIMESCOPE x %NI_PERC%

          <LS_SIGNEDDATA> = FACTOR11 * NI_PERC .
          <LS_RESOURCE_ACCT> = 'RP020011' .
           <LS_TIME> = LS_TIME-ID .
           FACTOR17 = <LS_SIGNEDDATA>. "CR3102
           COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

*****          <LS_SIGNEDDATA> = FACTOR11 * NI_PERC . "CR3102
*****          <LS_RESOURCE_ACCT> = 'AC710130' .  "CR3102
*****           <LS_TIME> = LS_TIME-ID .   "CR3102
*****           COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.  "CR3102

" GEN CALC 10 START FOR UK ONLY !!!!! "CR3102
       CLEAR LS_RESOURCE.
       READ TABLE LT_RESOURCE_TX
       INTO LS_RESOURCE
       WITH KEY
       AUDITTRAIL    = ORIG_AUDITTRAIL
       CATEGORY      = ORIG_CATEGORY
       EMPLOYEE      = ORIG_EMPLOYEE
       ENTITY        = ORIG_ENTITY
       RESOURCE_ACCT  = 'RP020003'
       RPTCURRENCY    = ORIG_CURRENCY
       TIME           = LS_TIME-ID
       BINARY SEARCH.
       IF SY-SUBRC = 0.
         FACTOR18 = LS_RESOURCE-SIGNEDDATA.
         ELSE.
         FACTOR18 = 0.
         ENDIF.

        <LS_SIGNEDDATA> = FACTOR18 * NI_PERC .
        FACTOR19 = <LS_SIGNEDDATA>.
        <LS_RESOURCE_ACCT> = 'RP020014' .
         <LS_TIME> = LS_TIME-ID .
         COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

        <LS_SIGNEDDATA> = FACTOR17 - FACTOR19.
        <LS_RESOURCE_ACCT> = 'AC710130' .
         <LS_TIME> = LS_TIME-ID .
         COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

        <LS_SIGNEDDATA> = FACTOR18 + FACTOR19.
        <LS_RESOURCE_ACCT> = 'AC710080' .
         <LS_TIME> = LS_TIME-ID .
         COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
" GEN CALC 10 END   FOR UK ONLY !!!!! "CR3102

"// Calculate FTE pension contribution

          <LS_SIGNEDDATA> = VAR_PENSIONSALARY * FACTOR12 .
          <LS_RESOURCE_ACCT> = 'RP020012' .
           <LS_TIME> = LS_TIME-ID .
           COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
*//Populate the correct Inc Statement Pension account
*Look up value in RP900004 in XXXX.INP"
"UK CALC 6
           IF FACTOR13 = 10.
"// Defined benefit
          <LS_RESOURCE_ACCT> = 'AC710149'  .
          <LS_SIGNEDDATA> = ( VAR_PENSIONSALARY * FACTOR12 ) + FACTOR6 . "CR NEW_002
           <LS_TIME> = LS_TIME-ID .
           COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

           <LS_SIGNEDDATA> = 0.
           <LS_RESOURCE_ACCT> = 'AC710148' .
           <LS_TIME> = LS_TIME-ID .
           COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

           <LS_RESOURCE_ACCT> = 'AC710151' .
           <LS_TIME> = LS_TIME-ID .
           COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

             ELSEIF FACTOR13 = 11.
"// Defined contribution
          <LS_RESOURCE_ACCT> = 'AC710148' .
          <LS_SIGNEDDATA> = ( VAR_PENSIONSALARY * FACTOR12 ) + FACTOR6 . "CR NEW_002
           <LS_TIME> = LS_TIME-ID .
           COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

           <LS_SIGNEDDATA> = 0.
           <LS_RESOURCE_ACCT> = 'AC710149' .
           <LS_TIME> = LS_TIME-ID .
           COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

           <LS_RESOURCE_ACCT> = 'AC710151' .
           <LS_TIME> = LS_TIME-ID .
           COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

             ELSEIF FACTOR13 = 12.
"// Auto Enrolment
          <LS_RESOURCE_ACCT> = 'AC710151' .
          <LS_SIGNEDDATA> = ( VAR_PENSIONSALARY * FACTOR12 ) + FACTOR6 . "CR NEW_002
           <LS_TIME> = LS_TIME-ID .
           COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

           <LS_SIGNEDDATA> = 0.
           <LS_RESOURCE_ACCT> = 'AC710148' .
           <LS_TIME> = LS_TIME-ID .
           COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

           <LS_RESOURCE_ACCT> = 'AC710149' .
           <LS_TIME> = LS_TIME-ID .
           COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

               ENDIF.

  ENDIF ."CALC IS ONLY FOR PERM EMP OR PERM ROLES

"//CALCULATE ITSS COST TO CONTRACT
****"ONLY FOR EMPLOYEES IN LT_ITSSROLES
 IF ITSS_FLAG = 'Y'."EMPLOYEE IS UNDER TOTAL_ITSSROLES
***    LOOP AT LT_ITSSROLES INTO WA_MEMBER.
***    FACTOR16 = FACTOR14.
    CLEAR LS_RESOURCE.
    READ TABLE LT_RESOURCE_TX
    INTO LS_RESOURCE
    WITH KEY
    AUDITTRAIL    = ORIG_AUDITTRAIL
    CATEGORY      = ORIG_CATEGORY
    EMPLOYEE      = ORIG_EMPLOYEE
    ENTITY        = ORIG_ENTITY
    RESOURCE_ACCT  = 'RP000431' "FACTOR14
    RPTCURRENCY    = ORIG_CURRENCY
    TIME           = LS_TIME-ID
    BINARY SEARCH.
    IF SY-SUBRC = 0 .
      FACTOR14 = LS_RESOURCE-SIGNEDDATA.
      ELSE.
        FACTOR14 = 0.
      ENDIF.

      CLEAR LS_RESOURCE.
      READ TABLE LT_RESOURCE_FLG
      INTO LS_RESOURCE
      WITH KEY
      AUDITTRAIL    = 'INPUT'
      CATEGORY      = WORKING_CATEGORY
      EMPLOYEE      = ORIG_EMPLOYEE
      ENTITY        = 'NO_PROFITCENTREUK'
      RESOURCE_ACCT  = 'RP000430'
      RPTCURRENCY    = 'LC'
      TIME           = YEAR_INP
      BINARY SEARCH.
      IF SY-SUBRC = 0 .
        VAR_ITSS_RATES = LS_RESOURCE-SIGNEDDATA.
        ELSE.
          VAR_ITSS_RATES = 0.
        ENDIF.

    <LS_SIGNEDDATA> = VAR_ITSS_RATES * FACTOR14 .
    FACTOR16 = <LS_SIGNEDDATA>.

*****  IF COMPANY = 'S101'. "REMOVE S101 SEP21
************************* 27.09.2017 UNCOMMENTED LINES CHANGE 4 START ***************************
        <LS_RESOURCE_ACCT> = 'AC710355'.  "'AC700006' .
        <LS_TIME> = LS_TIME-ID .
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            "BELOW UPDATING THE LT_RESOURCE_TX START
            CLEAR LS_RESOURCE.
            LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
            LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
            LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
            LS_RESOURCE-ENTITY      = <LS_ENTITY>.
            LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
            LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
            LS_RESOURCE-TIME          = <LS_TIME>.
            LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
            LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
              AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
              AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
              AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
              AND RPTCURRENCY = <LS_RPTCURRENCY> AND
              TIME = <LS_TIME>.
              <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
              ENDLOOP.
              IF SY-SUBRC <> 0.
                INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
                ENDIF.
            "ABOVE UPDATING THE LT_RESOURCE_TX END

        <LS_RESOURCE_ACCT> = 'RP020400' .
        <LS_TIME> = LS_TIME-ID .
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

        <LS_RESOURCE_ACCT> = 'AC220000' .
        <LS_TIME> = LS_TIME-ID .
        <LS_SIGNEDDATA> = FACTOR16 * -1 .
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
        <LS_SIGNEDDATA> = FACTOR16 .
************************* 27.09.2017 UNCOMMENTED LINES CHANGE 4 END ***************************
*****
*****    ELSE.

************************* 27.09.2017 COMMENT LINES CHANGE 4 START ***************************
*        <LS_RESOURCE_ACCT> = 'AC710355'.  "'AC700007' .
*        <LS_TIME> = LS_TIME-ID .
*        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
*            "BELOW UPDATING THE LT_RESOURCE_TX START
*            CLEAR LS_RESOURCE.
*            LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
*            LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
*            LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
*            LS_RESOURCE-ENTITY      = <LS_ENTITY>.
*            LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
*            LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
*            LS_RESOURCE-TIME          = <LS_TIME>.
*            LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
*            LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
*              AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
*              AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
*              AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
*              AND RPTCURRENCY = <LS_RPTCURRENCY> AND
*              TIME = <LS_TIME>.
*              <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
*              ENDLOOP.
*              IF SY-SUBRC <> 0.
*                INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
*                ENDIF.
*            "ABOVE UPDATING THE LT_RESOURCE_TX END
*
*        <LS_RESOURCE_ACCT> = 'RP020400' .
*        <LS_TIME> = LS_TIME-ID .
*        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
*
*****        <LS_RESOURCE_ACCT> = 'AC213010' . "change3
*****        <LS_TIME> = LS_TIME-ID .          "change3
*****        <LS_SIGNEDDATA> = FACTOR16 * -1 . "change3
*****        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>. "change3
*        <LS_SIGNEDDATA> = FACTOR16 .
*
******    ENDIF."REMOVE S101 SEP21
************************* 27.09.2017 COMMENT LINES CHANGE 4 END ***************************

    TIME_STRING = <LS_TIME>.
    TIME_STRING = TIME_STRING(4).

    IF TIME_STRING = CATEGORY_YEAR  AND WORKING_CATEGORY = 'WFORECAST'."ITSSCOSTS_UK

        <LS_RESOURCE_ACCT> = 'RP020400' .
        <LS_CATEGORY> = 'CY_FCST'.
        <LS_TIME> = 'XXXX.INP' .
        <LS_SIGNEDDATA> = FACTOR16 . "RP000430 * RP000431 FOR FIRST YEAR
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

        <LS_RESOURCE_ACCT> = 'RP000431' .
        <LS_CATEGORY> = 'CY_FCST'.
        <LS_TIME> = 'XXXX.INP' .
        <LS_SIGNEDDATA> = FACTOR14 ."SUM OF RP000431
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
      ENDIF.
      <LS_CATEGORY> = ORIG_CATEGORY.
      <LS_TIME> = LS_TIME-ID.

  ENDIF. "ITSS_FLAG ='Y'

     ENDLOOP."TIMESCOPE UK REGION END
ENDIF. " UK REGION
"UK SPECIFIC CALC END

"ME CALC SPECIFIC CALCULATION - START
IF ( RESOURCE_TYPE = 'E' AND EMPLOYEE_FLAG = 'Y' )  OR ( VAR_RP000425 = 0 AND ROLES_FLAG = 'Y' ). "CALC IS ONLY FOR PERM EMP OR PERM ROLES
IF REGION = 'ME'.

  CLEAR: FACTOR1 , FACTOR2, FACTOR3, FACTOR4 , FACTOR5, FACTOR6, FACTOR7,
  FACTOR8, FACTOR9 , FACTOR10 ,FACTOR11, FACTOR12, FACTOR13, FACTOR16,FACTOR14, FACTOR15 , FACTOR18, FACTOR19, FACTOR20.

  YEAR1_APR = YEAR_INP(4) && '.04' .
  YEAR2_APR = YEAR2_INP(4) && '.04' .

  READ TABLE LT_TIMESCOPE TRANSPORTING NO FIELDS
  WITH KEY
  ID = YEAR1_APR
  BINARY SEARCH.
  IF SY-SUBRC = 0.
    FACTOR19 = 1.
    ELSE.
      FACTOR19 = 0.
      ENDIF.

  READ TABLE LT_TIMESCOPE TRANSPORTING NO FIELDS
  WITH KEY
  ID = YEAR2_APR
  BINARY SEARCH.
  IF SY-SUBRC = 0.
    FACTOR20 = 1.
    ELSE.
      FACTOR20 = 0.
      ENDIF.

"ME CALC 1

  CLEAR LS_RESOURCE.
  READ TABLE LT_RESOURCE_TX
  INTO LS_RESOURCE
  WITH KEY
  AUDITTRAIL = ORIG_AUDITTRAIL
  CATEGORY   = ORIG_CATEGORY
  EMPLOYEE   = ORIG_EMPLOYEE
  ENTITY     = ORIG_ENTITY
  RESOURCE_ACCT  = 'RP020001'
  RPTCURRENCY    = ORIG_CURRENCY
  TIME           = YEAR_INP
  BINARY SEARCH.
  IF SY-SUBRC = 0.
    FACTOR1 = LS_RESOURCE-SIGNEDDATA.
  ELSE.
    FACTOR1 = 0.
    ENDIF.

  CLEAR LS_RESOURCE.
  READ TABLE LT_RESOURCE_TX
  INTO LS_RESOURCE
  WITH KEY
  AUDITTRAIL = ORIG_AUDITTRAIL
  CATEGORY   = ORIG_CATEGORY
  EMPLOYEE   = ORIG_EMPLOYEE
  ENTITY     = ORIG_ENTITY
  RESOURCE_ACCT  = 'RP020002'
  RPTCURRENCY    = ORIG_CURRENCY
  TIME           = YEAR_INP
  BINARY SEARCH.
  IF SY-SUBRC = 0.
    FACTOR2 = LS_RESOURCE-SIGNEDDATA.
  ELSE.
    FACTOR2 = 0.
    ENDIF.

  CLEAR LS_RESOURCE.
  READ TABLE LT_RESOURCE_FLG
  INTO LS_RESOURCE
  WITH KEY
  AUDITTRAIL    = ORIG_AUDITTRAIL
  CATEGORY      = ORIG_CATEGORY
  EMPLOYEE      = ORIG_EMPLOYEE
  ENTITY        = ORIG_ENTITY
  RESOURCE_ACCT  = 'RP010001'
  RPTCURRENCY    = ORIG_CURRENCY
  TIME           = YEAR_INP     "'XXXX.INP'
  BINARY SEARCH.
  IF SY-SUBRC = 0 .
    FACTOR3 =  LS_RESOURCE-SIGNEDDATA .
    ELSE.
    FACTOR3 = 0.
    ENDIF.

  CLEAR LS_RESOURCE.
  READ TABLE LT_RESOURCE_FLG
  INTO LS_RESOURCE
  WITH KEY
  AUDITTRAIL    = ORIG_AUDITTRAIL
  CATEGORY      = ORIG_CATEGORY
  EMPLOYEE      = ORIG_EMPLOYEE
  ENTITY        = ORIG_ENTITY
  RESOURCE_ACCT  = 'RP000132'
  RPTCURRENCY    = ORIG_CURRENCY
  TIME           = YEAR_INP
  BINARY SEARCH.
  IF SY-SUBRC = 0 .
    FACTOR4 =  LS_RESOURCE-SIGNEDDATA .
    ELSE.
    FACTOR4 = 0.
    ENDIF.

  CLEAR LS_RESOURCE.
  READ TABLE LT_RESOURCE_FLG
  INTO LS_RESOURCE
  WITH KEY
  AUDITTRAIL    = ORIG_AUDITTRAIL
  CATEGORY      = ORIG_CATEGORY
  EMPLOYEE      = ORIG_EMPLOYEE
  ENTITY        = ORIG_ENTITY
  RESOURCE_ACCT  = 'RP000133'
  RPTCURRENCY    = ORIG_CURRENCY
  TIME           = 'XXXX.INP'
  BINARY SEARCH.
  IF SY-SUBRC = 0 .
    FACTOR5 =  LS_RESOURCE-SIGNEDDATA .
    ELSE.
    FACTOR5 = 0.
    ENDIF.

  CLEAR LS_RESOURCE.
  READ TABLE LT_RESOURCE_TX
  INTO LS_RESOURCE
  WITH KEY
  AUDITTRAIL = ORIG_AUDITTRAIL
  CATEGORY   = ORIG_CATEGORY
  EMPLOYEE   = ORIG_EMPLOYEE
  ENTITY     = ORIG_ENTITY
  RESOURCE_ACCT  = 'RP020001'
  RPTCURRENCY    = ORIG_CURRENCY
  TIME           = YEAR2_INP
  BINARY SEARCH.
  IF SY-SUBRC = 0.
    FACTOR6 = LS_RESOURCE-SIGNEDDATA.
  ELSE.
    FACTOR6 = 0.
    ENDIF.

  CLEAR LS_RESOURCE.
  READ TABLE LT_RESOURCE_TX
  INTO LS_RESOURCE
  WITH KEY
  AUDITTRAIL = ORIG_AUDITTRAIL
  CATEGORY   = ORIG_CATEGORY
  EMPLOYEE   = ORIG_EMPLOYEE
  ENTITY     = ORIG_ENTITY
  RESOURCE_ACCT  = 'RP020002'
  RPTCURRENCY    = ORIG_CURRENCY
  TIME           = YEAR2_INP
  BINARY SEARCH.
  IF SY-SUBRC = 0.
    FACTOR7 = LS_RESOURCE-SIGNEDDATA.
  ELSE.
    FACTOR7 = 0.
    ENDIF.

  CLEAR LS_RESOURCE.
  READ TABLE LT_RESOURCE_FLG
  INTO LS_RESOURCE
  WITH KEY
  AUDITTRAIL    = ORIG_AUDITTRAIL
  CATEGORY      = ORIG_CATEGORY
  EMPLOYEE      = ORIG_EMPLOYEE
  ENTITY        = ORIG_ENTITY
  RESOURCE_ACCT  = 'RP000135'
  RPTCURRENCY    = ORIG_CURRENCY
  TIME           = 'XXXX.INP'
  BINARY SEARCH.
  IF SY-SUBRC = 0 .
    FACTOR8 =  LS_RESOURCE-SIGNEDDATA .
    ELSE.
    FACTOR8 = 0.
    ENDIF.

    CLEAR LS_RESOURCE.
    READ TABLE LT_RESOURCE_FLG
    INTO LS_RESOURCE
    WITH KEY
    AUDITTRAIL    = ORIG_AUDITTRAIL
    CATEGORY      = ORIG_CATEGORY
    EMPLOYEE      = 'NO_EMPLOYEE'
    ENTITY        = PROFIT_CENTRE
    RESOURCE_ACCT  = 'RP000136'
    RPTCURRENCY    = ORIG_CURRENCY
    TIME           = YEAR_INP
    BINARY SEARCH.
    IF SY-SUBRC = 0 .
      FACTOR9 =  LS_RESOURCE-SIGNEDDATA .
      ELSE.
      FACTOR9 = 0.
      ENDIF.

"// ME CALC 1 TERMINAL GRATUITY

"YEAR 1 CALCULATION
   IF FACTOR19 = 1.
      IF FACTOR1 = 0 AND FACTOR2 = 0.
          <LS_RESOURCE_ACCT> = 'AC710160'  .
          <LS_TIME> = YEAR_INP(4) && '.04' .
          <LS_SIGNEDDATA> = 0 .
          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
        ELSE.
          IF FACTOR4 = 1.
              <LS_RESOURCE_ACCT> = 'AC710160'  .
              <LS_TIME> = YEAR_INP(4) && '.04' .
              <LS_SIGNEDDATA> =  ( FACTOR1 + FACTOR2 )  * FACTOR5 .
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            ELSE.
              <LS_RESOURCE_ACCT> = 'AC710160'  .
              <LS_TIME> = YEAR_INP(4) && '.04' .
              <LS_SIGNEDDATA> = ( ( FACTOR1 + FACTOR2 ) - FACTOR3 ) * FACTOR4 * FACTOR5.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
              ENDIF.
          ENDIF.
        ENDIF.
"YEAR 2 CALCULATION

   IF FACTOR20 = 1.
      FACTOR18 = FACTOR4 + 1.
      IF FACTOR6 = 0 AND FACTOR7 = 0.
          <LS_RESOURCE_ACCT> = 'AC710160'  .
          <LS_TIME> = YEAR2_INP(4) && '.04' .
          <LS_SIGNEDDATA> = 0 .
          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
        ELSE.
          IF FACTOR18 = 2 AND FACTOR1 = 0 .
            <LS_RESOURCE_ACCT> = 'AC710160'  .
            <LS_TIME> = YEAR2_INP(4) && '.04' .
            <LS_SIGNEDDATA> =  ( FACTOR6 + FACTOR7 )  * FACTOR5.
            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            ELSE.
            <LS_RESOURCE_ACCT> = 'AC710160'  .
            <LS_TIME> = YEAR2_INP(4) && '.04' .
            <LS_SIGNEDDATA> = ( ( FACTOR6 + FACTOR7 ) - (  FACTOR1 + FACTOR2  ) ) * ( FACTOR4 + 1 ) * FACTOR5.
            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            ENDIF.
        ENDIF.
      ENDIF.

"// ME CALC 2 PENSION
    CLEAR LS_RESOURCE.
    READ TABLE LT_RESOURCE_FLG
    INTO LS_RESOURCE
    WITH KEY
    AUDITTRAIL    = ORIG_AUDITTRAIL
    CATEGORY      = ORIG_CATEGORY
    EMPLOYEE      = ORIG_EMPLOYEE
    ENTITY        = ORIG_ENTITY
    RESOURCE_ACCT  = 'RP000134'
    RPTCURRENCY    = ORIG_CURRENCY
    TIME           = 'XXXX.INP'
    BINARY SEARCH.
    IF SY-SUBRC = 0 .
      FACTOR12 =  LS_RESOURCE-SIGNEDDATA .
      ELSE.
      FACTOR12 = 0.
      ENDIF.

    CLEAR LS_RESOURCE.
    READ TABLE LT_RESOURCE_FLG
    INTO LS_RESOURCE
    WITH KEY
    AUDITTRAIL    = ORIG_AUDITTRAIL
    CATEGORY      = ORIG_CATEGORY
    EMPLOYEE      = ORIG_EMPLOYEE
    ENTITY        = ORIG_ENTITY
    RESOURCE_ACCT  = 'RP010020' "FACTOR16
    RPTCURRENCY    = ORIG_CURRENCY
    TIME           = YEAR_INP
    BINARY SEARCH.
    IF SY-SUBRC = 0 .
      FACTOR16 =  LS_RESOURCE-SIGNEDDATA .
      ELSE.
      FACTOR16 = 0.
      ENDIF.

    CLEAR LS_RESOURCE.
    READ TABLE LT_RESOURCE_FLG
    INTO LS_RESOURCE
    WITH KEY
    AUDITTRAIL    = ORIG_AUDITTRAIL
    CATEGORY      = ORIG_CATEGORY
    EMPLOYEE      = ORIG_EMPLOYEE
    ENTITY        = ORIG_ENTITY
    RESOURCE_ACCT  = 'RP010021' "FACTOR17
    RPTCURRENCY    = ORIG_CURRENCY
    TIME           = YEAR_INP
    BINARY SEARCH.
    IF SY-SUBRC = 0 .
      FACTOR17 =  LS_RESOURCE-SIGNEDDATA .
      ELSE.
      FACTOR17 = 0.
      ENDIF.

    CLEAR LS_RESOURCE.
    READ TABLE LT_RESOURCE_FLG
    INTO LS_RESOURCE
    WITH KEY
    AUDITTRAIL    = ORIG_AUDITTRAIL
    CATEGORY      = ORIG_CATEGORY
    EMPLOYEE      = ORIG_EMPLOYEE
    ENTITY        = ORIG_ENTITY
    RESOURCE_ACCT  = 'RP000131'
    RPTCURRENCY    = ORIG_CURRENCY
    TIME           = 'XXXX.INP'
    BINARY SEARCH.
      IF SY-SUBRC = 0 .
      FACTOR13 =  LS_RESOURCE-SIGNEDDATA .
      ELSE.
      FACTOR13 = 0.
      ENDIF.

  IF FACTOR13 = 1.
      EMP_COMPANY = 'EMP_' && COMPANY.
      CLEAR LS_RESOURCE.
      READ TABLE LT_RESOURCE_FLG
      INTO LS_RESOURCE
      WITH KEY
      AUDITTRAIL    = 'INP_CAPPED'
      CATEGORY      = ORIG_CATEGORY
      EMPLOYEE      = EMP_COMPANY
      ENTITY        = 'NO_PROFITCENTREME'
      RESOURCE_ACCT  = 'RP000120'
      RPTCURRENCY    = ORIG_CURRENCY
      TIME           = YEAR_INP
      BINARY SEARCH.
      IF SY-SUBRC = 0 .
        INP_CAPPED =  LS_RESOURCE-SIGNEDDATA .
        ELSE.
        INP_CAPPED  = 0.
        ENDIF.

  ELSEIF FACTOR13 = 2.
      EMP_COMPANY = 'EMP_' && COMPANY.
      CLEAR LS_RESOURCE.
      READ TABLE LT_RESOURCE_FLG
      INTO LS_RESOURCE
      WITH KEY
      AUDITTRAIL    = 'INP_CAPPED'
      CATEGORY      = ORIG_CATEGORY
      EMPLOYEE      = EMP_COMPANY
      ENTITY        = 'NO_PROFITCENTREME'
      RESOURCE_ACCT  = 'RP000121'
      RPTCURRENCY    = ORIG_CURRENCY
      TIME           = YEAR_INP
      BINARY SEARCH.
      IF SY-SUBRC = 0 .
        INP_CAPPED =  LS_RESOURCE-SIGNEDDATA .
        ELSE.
        INP_CAPPED  = 0.
        ENDIF.

    ELSE.
      INP_CAPPED  = 0.

      ENDIF.

    IF FACTOR13 = 1 AND ( COMPANY = 'S130' OR COMPANY = 'S131' OR COMPANY = 'S132' OR COMPANY = 'S133' OR COMPANY = 'S137' ).

    "//YEAR 1
    <LS_RESOURCE_ACCT> = 'RP020013'  .
    <LS_TIME> = YEAR_INP.
    <LS_SIGNEDDATA> = ( FACTOR1 + FACTOR2 + VAR_PENSIONALLOW + FACTOR16 + FACTOR17 )  * FACTOR12.
    FACTOR14 = <LS_SIGNEDDATA> .
    COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

    "//YEAR 2
    <LS_RESOURCE_ACCT> = 'RP020013'  .
    <LS_TIME> = YEAR2_INP.
    <LS_SIGNEDDATA> = ( FACTOR6 + FACTOR7 + VAR_PENSIONALLOW )  * FACTOR12.
    FACTOR15 = <LS_SIGNEDDATA> .
    COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

    "//YEAR 1
    IF FACTOR14 <= INP_CAPPED.
      <LS_RESOURCE_ACCT> = 'RP010012'  .
      <LS_TIME> = YEAR_INP.
      <LS_SIGNEDDATA> = FACTOR14.
      COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
     ELSE.
      <LS_RESOURCE_ACCT> = 'RP010012'  .
      <LS_TIME> = YEAR_INP.
      <LS_SIGNEDDATA> = INP_CAPPED.
      COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
     ENDIF.

    "//YEAR 2
    IF FACTOR15 <= INP_CAPPED.
      <LS_RESOURCE_ACCT> = 'RP010012'  .
      <LS_TIME> = YEAR2_INP.
      <LS_SIGNEDDATA> = FACTOR15.
      COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
     ELSE.
      <LS_RESOURCE_ACCT> = 'RP010012'  .
      <LS_TIME> = YEAR2_INP.
      <LS_SIGNEDDATA> = INP_CAPPED.
      COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
     ENDIF.

  LOOP AT LT_TIMESCOPE INTO LS_TIME.

    CLEAR: LS_RESOURCE.
    READ TABLE LT_RESOURCE_TX
    INTO LS_RESOURCE
    WITH KEY
    AUDITTRAIL = ORIG_AUDITTRAIL
    CATEGORY   = ORIG_CATEGORY
    EMPLOYEE   = ORIG_EMPLOYEE
    ENTITY     = ORIG_ENTITY
    RESOURCE_ACCT  = 'RP000408' "FTE
    RPTCURRENCY    = ORIG_CURRENCY
    TIME           = LS_TIME-ID
    BINARY SEARCH.
    IF SY-SUBRC = 0.
      FTE_408 = LS_RESOURCE-SIGNEDDATA.
    ELSE.
      FTE_408 = 0.
      ENDIF.

    IF FTE_408 <> 0.

    IF LS_TIME-ID(4) = YEAR_INP(4)."FIRST YEAR.
       IF FACTOR14 <= INP_CAPPED.
        <LS_RESOURCE_ACCT> = 'AC710152'  .
        <LS_TIME> = LS_TIME-ID.
        <LS_SIGNEDDATA> = FACTOR14 / 12.
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
       ELSE.
        <LS_RESOURCE_ACCT> = 'AC710152'  .
        <LS_TIME> = LS_TIME-ID.
        <LS_SIGNEDDATA> = INP_CAPPED / 12.
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
       ENDIF.

      ELSE.
       IF FACTOR15 <= INP_CAPPED.
        <LS_RESOURCE_ACCT> = 'AC710152'  .
        <LS_TIME> = LS_TIME-ID.
        <LS_SIGNEDDATA> = FACTOR15 / 12.
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
       ELSE.
        <LS_RESOURCE_ACCT> = 'AC710152'  .
        <LS_TIME> = LS_TIME-ID.
        <LS_SIGNEDDATA> = INP_CAPPED / 12.
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
       ENDIF.

        ENDIF.
        ENDIF. "IF FTE_408 <> 0.

    ENDLOOP.

  ELSEIF FACTOR13 = 1 AND ( COMPANY = 'S135'  ).

    "//YEAR 1
    <LS_RESOURCE_ACCT> = 'RP020013'  .
    <LS_TIME> = YEAR_INP.
    <LS_SIGNEDDATA> = ( FACTOR1 + FACTOR2  )  * FACTOR12.
    FACTOR14 = <LS_SIGNEDDATA> .
    COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

    "//YEAR 2
    <LS_RESOURCE_ACCT> = 'RP020013'  .
    <LS_TIME> = YEAR2_INP.
    <LS_SIGNEDDATA> = ( FACTOR6 + FACTOR7  )  * FACTOR12.
    FACTOR15 = <LS_SIGNEDDATA> .
    COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

  LOOP AT LT_TIMESCOPE INTO LS_TIME.
    CLEAR: LS_RESOURCE.
    READ TABLE LT_RESOURCE_TX
    INTO LS_RESOURCE
    WITH KEY
    AUDITTRAIL = ORIG_AUDITTRAIL
    CATEGORY   = ORIG_CATEGORY
    EMPLOYEE   = ORIG_EMPLOYEE
    ENTITY     = ORIG_ENTITY
    RESOURCE_ACCT  = 'RP000408' "FTE
    RPTCURRENCY    = ORIG_CURRENCY
    TIME           = LS_TIME-ID
    BINARY SEARCH.
    IF SY-SUBRC = 0.
      FTE_408 = LS_RESOURCE-SIGNEDDATA.
    ELSE.
      FTE_408 = 0.
      ENDIF.

    IF FTE_408 <> 0.
    IF LS_TIME-ID(4) = YEAR_INP(4)."FIRST YEAR.
        <LS_RESOURCE_ACCT> = 'AC710152'  .
        <LS_TIME> = LS_TIME-ID.
        <LS_SIGNEDDATA> = FACTOR14 / 12.
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
      ELSE.
        <LS_RESOURCE_ACCT> = 'AC710152'  .
        <LS_TIME> = LS_TIME-ID.
        <LS_SIGNEDDATA> = FACTOR15 / 12.
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

        ENDIF.
        ENDIF. "IF FTE_408 <> 0.
    ENDLOOP.

  ELSEIF FACTOR13 = 1 AND ( COMPANY = 'S138' OR COMPANY = 'S245' ).
    "//YEAR 1
    <LS_RESOURCE_ACCT> = 'RP020013'  .
    <LS_TIME> = YEAR_INP.
    <LS_SIGNEDDATA> = ( FACTOR1 + FACTOR2 + FACTOR16  )  * FACTOR12.
    FACTOR14 = <LS_SIGNEDDATA> .
    COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

    "//YEAR 2
    <LS_RESOURCE_ACCT> = 'RP020013'  .
    <LS_TIME> = YEAR2_INP.
    <LS_SIGNEDDATA> = ( FACTOR6 + FACTOR7 + FACTOR16  )  * FACTOR12.
    FACTOR15 = <LS_SIGNEDDATA> .
    COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

    LOOP AT LT_TIMESCOPE INTO LS_TIME.
    CLEAR: LS_RESOURCE.
    READ TABLE LT_RESOURCE_TX
    INTO LS_RESOURCE
    WITH KEY
    AUDITTRAIL = ORIG_AUDITTRAIL
    CATEGORY   = ORIG_CATEGORY
    EMPLOYEE   = ORIG_EMPLOYEE
    ENTITY     = ORIG_ENTITY
    RESOURCE_ACCT  = 'RP000408' "FTE
    RPTCURRENCY    = ORIG_CURRENCY
    TIME           = LS_TIME-ID
    BINARY SEARCH.
    IF SY-SUBRC = 0.
      FTE_408 = LS_RESOURCE-SIGNEDDATA.
    ELSE.
      FTE_408 = 0.
      ENDIF.

    IF FTE_408 <> 0.
      IF LS_TIME-ID(4) = YEAR_INP(4)."FIRST YEAR.
          <LS_RESOURCE_ACCT> = 'AC710152'  .
          <LS_TIME> = LS_TIME-ID.
          <LS_SIGNEDDATA> = FACTOR14 / 12.
          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
        ELSE.
          <LS_RESOURCE_ACCT> = 'AC710152'  .
          <LS_TIME> = LS_TIME-ID.
          <LS_SIGNEDDATA> = FACTOR15 / 12.
          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

          ENDIF.
          ENDIF. "IF FTE_408 <> 0.
      ENDLOOP.

  ELSEIF FACTOR13 = 2 AND ( COMPANY = 'S133'  ).
  "//PENSION FOR EXPATS

    "//YEAR 1
    <LS_RESOURCE_ACCT> = 'RP020013'  .
    <LS_TIME> = YEAR_INP.
    <LS_SIGNEDDATA> = ( FACTOR1 + FACTOR2 + VAR_PENSIONALLOW + FACTOR16 + FACTOR17 )  * FACTOR12.
    FACTOR14 = <LS_SIGNEDDATA> .
    COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

    "//YEAR 2
    <LS_RESOURCE_ACCT> = 'RP020013'  .
    <LS_TIME> = YEAR2_INP.
    <LS_SIGNEDDATA> = ( FACTOR6 + FACTOR7 + VAR_PENSIONALLOW )  * FACTOR12.
    FACTOR15 = <LS_SIGNEDDATA> .
    COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.


    "//YEAR 1
    IF FACTOR14 <= INP_CAPPED.
      <LS_RESOURCE_ACCT> = 'RP010012'  .
      <LS_TIME> = YEAR_INP.
      <LS_SIGNEDDATA> = FACTOR14.
      COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
     ELSE.
      <LS_RESOURCE_ACCT> = 'RP010012'  .
      <LS_TIME> = YEAR_INP.
      <LS_SIGNEDDATA> = INP_CAPPED.
      COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
     ENDIF.

    "//YEAR 2
    IF FACTOR15 <= INP_CAPPED.
      <LS_RESOURCE_ACCT> = 'RP010012'  .
      <LS_TIME> = YEAR2_INP.
      <LS_SIGNEDDATA> = FACTOR15.
      COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
     ELSE.
      <LS_RESOURCE_ACCT> = 'RP010012'  .
      <LS_TIME> = YEAR2_INP.
      <LS_SIGNEDDATA> = INP_CAPPED.
      COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
     ENDIF.

  LOOP AT LT_TIMESCOPE INTO LS_TIME.

    CLEAR: LS_RESOURCE.
    READ TABLE LT_RESOURCE_TX
    INTO LS_RESOURCE
    WITH KEY
    AUDITTRAIL = ORIG_AUDITTRAIL
    CATEGORY   = ORIG_CATEGORY
    EMPLOYEE   = ORIG_EMPLOYEE
    ENTITY     = ORIG_ENTITY
    RESOURCE_ACCT  = 'RP000408' "FTE
    RPTCURRENCY    = ORIG_CURRENCY
    TIME           = LS_TIME-ID
    BINARY SEARCH.
    IF SY-SUBRC = 0.
      FTE_408 = LS_RESOURCE-SIGNEDDATA.
    ELSE.
      FTE_408 = 0.
      ENDIF.

    IF FTE_408 <> 0.
    IF LS_TIME-ID(4) = YEAR_INP(4)."FIRST YEAR.
       IF FACTOR14 <= INP_CAPPED.
        <LS_RESOURCE_ACCT> = 'AC710152'  .
        <LS_TIME> = LS_TIME-ID.
        <LS_SIGNEDDATA> = FACTOR14 / 12.
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
       ELSE.
        <LS_RESOURCE_ACCT> = 'AC710152'  .
        <LS_TIME> = LS_TIME-ID.
        <LS_SIGNEDDATA> = INP_CAPPED / 12.
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
       ENDIF.

      ELSE.
       IF FACTOR15 <= INP_CAPPED.
        <LS_RESOURCE_ACCT> = 'AC710152'  .
        <LS_TIME> = LS_TIME-ID.
        <LS_SIGNEDDATA> = FACTOR15 / 12.
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
       ELSE.
        <LS_RESOURCE_ACCT> = 'AC710152'  .
        <LS_TIME> = LS_TIME-ID.
        <LS_SIGNEDDATA> = INP_CAPPED / 12.
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
       ENDIF.

        ENDIF.
        ENDIF. "IF FTE_408 <> 0.

    ENDLOOP.

  ELSEIF FACTOR13 = 2 AND ( COMPANY = 'S138' OR COMPANY = 'S245' ).
  "//PENSION FOR EXPATS

    "//YEAR 1
    <LS_RESOURCE_ACCT> = 'RP020013'  .
    <LS_TIME> = YEAR_INP.
    <LS_SIGNEDDATA> = ( FACTOR1 + FACTOR2 + FACTOR16 + FACTOR17  )  * FACTOR12.
    FACTOR14 = <LS_SIGNEDDATA> .
    COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

    "//YEAR 2
    <LS_RESOURCE_ACCT> = 'RP020013'  .
    <LS_TIME> = YEAR2_INP.
    <LS_SIGNEDDATA> = ( FACTOR6 + FACTOR7 + FACTOR16 + FACTOR17  )  * FACTOR12.
    FACTOR15 = <LS_SIGNEDDATA> .
    COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

    LOOP AT LT_TIMESCOPE INTO LS_TIME.
    CLEAR: LS_RESOURCE.
    READ TABLE LT_RESOURCE_TX
    INTO LS_RESOURCE
    WITH KEY
    AUDITTRAIL = ORIG_AUDITTRAIL
    CATEGORY   = ORIG_CATEGORY
    EMPLOYEE   = ORIG_EMPLOYEE
    ENTITY     = ORIG_ENTITY
    RESOURCE_ACCT  = 'RP000408' "FTE
    RPTCURRENCY    = ORIG_CURRENCY
    TIME           = LS_TIME-ID
    BINARY SEARCH.
    IF SY-SUBRC = 0.
      FTE_408 = LS_RESOURCE-SIGNEDDATA.
    ELSE.
      FTE_408 = 0.
      ENDIF.

    IF FTE_408 <> 0.
      IF LS_TIME-ID(4) = YEAR_INP(4)."FIRST YEAR.
          <LS_RESOURCE_ACCT> = 'AC710152'  .
          <LS_TIME> = LS_TIME-ID.
          <LS_SIGNEDDATA> = FACTOR14 / 12.
          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
        ELSE.
          <LS_RESOURCE_ACCT> = 'AC710152'  .
          <LS_TIME> = LS_TIME-ID.
          <LS_SIGNEDDATA> = FACTOR15 / 12.
          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

          ENDIF.
          ENDIF. "IF FTE_408 <> 0.
      ENDLOOP.

      ENDIF.

"// ME CALC 3 SORAS

    "YEAR 1 CALCULATION
    IF FACTOR19 = 1.
      IF  FACTOR1 = 0 AND FACTOR2 = 0 .

          <LS_RESOURCE_ACCT> = 'AC710165'  .
          <LS_TIME> = YEAR_INP(4) && '.04' .
          <LS_SIGNEDDATA> = 0.
          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

        ELSE.
        IF FACTOR13 = 2 AND ( COMPANY = 'S131' OR COMPANY = 'S132' OR COMPANY = 'S133' ).

          IF FACTOR4 = 1.
              <LS_RESOURCE_ACCT> = 'AC710165'  .
              <LS_TIME> = YEAR_INP(4) && '.04' .
              <LS_SIGNEDDATA> =  ( FACTOR1 + FACTOR2 ) * FACTOR8.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            ELSE.
              <LS_RESOURCE_ACCT> = 'AC710165'  .
              <LS_TIME> = YEAR_INP(4) && '.04' .
              <LS_SIGNEDDATA> = ( ( FACTOR1 + FACTOR2 ) - FACTOR3 ) * FACTOR8.
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            ENDIF.

            ELSEIF FACTOR13 = 1 AND ( COMPANY = 'S131' OR COMPANY = 'S132' OR COMPANY = 'S133' ).
            <LS_RESOURCE_ACCT> = 'AC710165'  .
            <LS_TIME> = YEAR_INP(4) && '.04' .
            <LS_SIGNEDDATA> = 0 .
            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

            ENDIF.
        ENDIF.
      ENDIF.


    "YEAR 2 CALCULATION
    IF FACTOR20 = 1.
      IF FACTOR6 = 0 AND FACTOR7 = 0.
          <LS_RESOURCE_ACCT> = 'AC710165'  .
          <LS_TIME> = YEAR2_INP(4) && '.04' .
          <LS_SIGNEDDATA> = 0.
          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

        ELSE.
        IF FACTOR13 = 2 AND ( COMPANY = 'S131' OR COMPANY = 'S132' OR COMPANY = 'S133' ).

          IF FACTOR18 = 2 AND FACTOR1 = 0 .
              <LS_RESOURCE_ACCT> = 'AC710165'  .
              <LS_TIME> = YEAR2_INP(4) && '.04' .
              <LS_SIGNEDDATA> =  ( FACTOR6 + FACTOR7 )  * FACTOR8 .
              COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            ELSE.
            <LS_RESOURCE_ACCT> = 'AC710165'  .
            <LS_TIME> = YEAR2_INP(4) && '.04' .
            <LS_SIGNEDDATA> = ( ( FACTOR6 + FACTOR7 ) - (  FACTOR1 + FACTOR2  ) ) * FACTOR8.
            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

            ENDIF.


            ELSEIF FACTOR13 = 1 AND ( COMPANY = 'S131' OR COMPANY = 'S132' OR COMPANY = 'S133' ).

            <LS_RESOURCE_ACCT> = 'AC710165'  .
            <LS_TIME> = YEAR2_INP(4) && '.04' .
            <LS_SIGNEDDATA> = 0 .
            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

            ENDIF.

          ENDIF.
        ENDIF.

"// ME CALC 4 WORKERS COMPENSATION
LOOP AT LT_TIMESCOPE INTO LS_TIME.
    CLEAR: LS_RESOURCE.
    READ TABLE LT_RESOURCE_TX
    INTO LS_RESOURCE
    WITH KEY
    AUDITTRAIL = ORIG_AUDITTRAIL
    CATEGORY   = ORIG_CATEGORY
    EMPLOYEE   = ORIG_EMPLOYEE
    ENTITY     = ORIG_ENTITY
    RESOURCE_ACCT  = 'RP020001'
    RPTCURRENCY    = ORIG_CURRENCY
    TIME           = LS_TIME-ID
    BINARY SEARCH.
    IF SY-SUBRC = 0.
      FACTOR10 = LS_RESOURCE-SIGNEDDATA.
    ELSE.
      FACTOR10 = 0.
    ENDIF.
    CLEAR: LS_RESOURCE.
    READ TABLE LT_RESOURCE_TX
    INTO LS_RESOURCE
    WITH KEY
    AUDITTRAIL = ORIG_AUDITTRAIL
    CATEGORY   = ORIG_CATEGORY
    EMPLOYEE   = ORIG_EMPLOYEE
    ENTITY     = ORIG_ENTITY
    RESOURCE_ACCT  = 'RP020002'
    RPTCURRENCY    = ORIG_CURRENCY
    TIME           = LS_TIME-ID
    BINARY SEARCH.
    IF SY-SUBRC = 0.
      FACTOR11 = LS_RESOURCE-SIGNEDDATA.
    ELSE.
      FACTOR11 = 0.
    ENDIF.

    <LS_SIGNEDDATA>     = ( FACTOR10 + FACTOR11 ) * FACTOR9 .
    <LS_RESOURCE_ACCT>  = 'AC710170' .
    <LS_TIME>           = LS_TIME-ID.
    COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
  ENDLOOP.

  ENDIF. " ME REGION
  ENDIF. "CALC IS ONLY FOR PERM EMP OR PERM ROLES
"ME SPECIFIC CALCULATION - END

"//ASPAC SPECIFIC CALCULATION - START
IF ( RESOURCE_TYPE = 'E' AND EMPLOYEE_FLAG = 'Y' )  OR ( VAR_RP000425 = 0 AND ROLES_FLAG = 'Y' ). "CALC IS ONLY FOR PERM EMP OR PERM ROLES
IF REGION = 'ASPAC'.

CLEAR: FACTOR1 , FACTOR2, FACTOR3, FACTOR4 , FACTOR5, FACTOR6, FACTOR7,
FACTOR8, FACTOR9 , FACTOR10 ,FACTOR11, FACTOR12, FACTOR13.
CLEAR LS_RESOURCE.

  CLEAR: LS_RESOURCE.
  READ TABLE LT_RESOURCE_FLG
  INTO LS_RESOURCE
  WITH KEY
  AUDITTRAIL = ORIG_AUDITTRAIL
  CATEGORY   = ORIG_CATEGORY
  EMPLOYEE   = ORIG_EMPLOYEE
  ENTITY     = ORIG_ENTITY
  RESOURCE_ACCT  = 'RP000200' "FACTOR1
  RPTCURRENCY    = ORIG_CURRENCY
  TIME           = 'XXXX.INP'
  BINARY SEARCH.
  IF SY-SUBRC = 0.
    FACTOR1 = LS_RESOURCE-SIGNEDDATA.
  ELSE.
    FACTOR1 = 0.
  ENDIF.

  CLEAR: LS_RESOURCE.
  READ TABLE LT_RESOURCE_FLG
  INTO LS_RESOURCE
  WITH KEY
  AUDITTRAIL = ORIG_AUDITTRAIL
  CATEGORY   = ORIG_CATEGORY
  EMPLOYEE   = ORIG_EMPLOYEE
  ENTITY     = ORIG_ENTITY
  RESOURCE_ACCT  = 'RP000201' "FACTOR2
  RPTCURRENCY    = ORIG_CURRENCY
  TIME           = 'XXXX.INP'
  BINARY SEARCH.
  IF SY-SUBRC = 0.
    FACTOR2 = LS_RESOURCE-SIGNEDDATA.
  ELSE.
    FACTOR2 = 0.
  ENDIF.

  CLEAR: LS_RESOURCE.
  READ TABLE LT_RESOURCE_FLG
  INTO LS_RESOURCE
  WITH KEY
  AUDITTRAIL = ORIG_AUDITTRAIL
  CATEGORY   = ORIG_CATEGORY
  EMPLOYEE   = ORIG_EMPLOYEE
  ENTITY     = ORIG_ENTITY
  RESOURCE_ACCT  = 'RP000202' "FACTOR3
  RPTCURRENCY    = ORIG_CURRENCY
  TIME           = 'XXXX.INP'
  BINARY SEARCH.
  IF SY-SUBRC = 0.
    FACTOR3 = LS_RESOURCE-SIGNEDDATA.
  ELSE.
    FACTOR3 = 0.
  ENDIF.

  CLEAR: LS_RESOURCE.
  READ TABLE LT_RESOURCE_FLG
  INTO LS_RESOURCE
  WITH KEY
  AUDITTRAIL = ORIG_AUDITTRAIL
  CATEGORY   = ORIG_CATEGORY
  EMPLOYEE   = 'NO_EMPLOYEE'
  ENTITY     = PROFIT_CENTRE  "ORIG_ENTITY
  RESOURCE_ACCT  = 'RP000210' "FACTOR4
  RPTCURRENCY    = ORIG_CURRENCY
  TIME           = YEAR_INP
  BINARY SEARCH.
  IF SY-SUBRC = 0.
    FACTOR4 = LS_RESOURCE-SIGNEDDATA.
  ELSE.
    FACTOR4 = 0.
  ENDIF.

  CLEAR: LS_RESOURCE.
  READ TABLE LT_RESOURCE_FLG
  INTO LS_RESOURCE
  WITH KEY
  AUDITTRAIL = ORIG_AUDITTRAIL
  CATEGORY   = ORIG_CATEGORY
  EMPLOYEE   = 'NO_EMPLOYEE'
  ENTITY     = PROFIT_CENTRE  "ORIG_ENTITY
  RESOURCE_ACCT  = 'RP000211' "FACTOR5
  RPTCURRENCY    = ORIG_CURRENCY
  TIME           = YEAR_INP
  BINARY SEARCH.
  IF SY-SUBRC = 0.
    FACTOR5 = LS_RESOURCE-SIGNEDDATA.
  ELSE.
    FACTOR5 = 0.
  ENDIF.

  READ TABLE LT_RESOURCE_FLG
  INTO LS_RESOURCE
  WITH KEY
  AUDITTRAIL = ORIG_AUDITTRAIL
  CATEGORY   = ORIG_CATEGORY
  EMPLOYEE   = 'NO_EMPLOYEE'
  ENTITY     = PROFIT_CENTRE  "ORIG_ENTITY
  RESOURCE_ACCT  = 'RP000212' "FACTOR6
  RPTCURRENCY    = ORIG_CURRENCY
  TIME           = YEAR_INP
  BINARY SEARCH.
  IF SY-SUBRC = 0.
    FACTOR6 = LS_RESOURCE-SIGNEDDATA.
  ELSE.
    FACTOR6 = 0.
  ENDIF.

  LOOP AT LT_TIMESCOPE INTO LS_TIME.

    CLEAR: LS_RESOURCE, FACTOR7.
    READ TABLE LT_RESOURCE_TX
    INTO LS_RESOURCE
    WITH KEY
    AUDITTRAIL = ORIG_AUDITTRAIL
    CATEGORY   = ORIG_CATEGORY
    EMPLOYEE   = ORIG_EMPLOYEE
    ENTITY     = ORIG_ENTITY
    RESOURCE_ACCT  = 'RP020001'
    RPTCURRENCY    = ORIG_CURRENCY
    TIME           = LS_TIME-ID
    BINARY SEARCH.
    IF SY-SUBRC = 0.
      FACTOR7 = LS_RESOURCE-SIGNEDDATA.
    ELSE.
      FACTOR7 = 0.
    ENDIF.

    CLEAR: LS_RESOURCE, FACTOR8.
    READ TABLE LT_RESOURCE_TX
    INTO LS_RESOURCE
    WITH KEY
    AUDITTRAIL = ORIG_AUDITTRAIL
    CATEGORY   = ORIG_CATEGORY
    EMPLOYEE   = ORIG_EMPLOYEE
    ENTITY     = ORIG_ENTITY
    RESOURCE_ACCT  = 'RP020002'
    RPTCURRENCY    = ORIG_CURRENCY
    TIME           = LS_TIME-ID
    BINARY SEARCH.
    IF SY-SUBRC = 0.
      FACTOR8 = LS_RESOURCE-SIGNEDDATA.
    ELSE.
      FACTOR8 = 0.
    ENDIF.

    CLEAR: LS_RESOURCE, FACTOR9.
    READ TABLE LT_RESOURCE_TX
    INTO LS_RESOURCE
    WITH KEY
    AUDITTRAIL = ORIG_AUDITTRAIL
    CATEGORY   = ORIG_CATEGORY
    EMPLOYEE   = ORIG_EMPLOYEE
    ENTITY     = ORIG_ENTITY
    RESOURCE_ACCT  = 'RP000408' "FTE
    RPTCURRENCY    = ORIG_CURRENCY
    TIME           = LS_TIME-ID
    BINARY SEARCH.
    IF SY-SUBRC = 0.
      FACTOR9 = LS_RESOURCE-SIGNEDDATA.
    ELSE.
      FACTOR9 = 0.
      ENDIF.

      FACTOR9 = 1.

"WORKERS COMPENSATION
***    IF FACTOR1 = 1.
        <LS_RESOURCE_ACCT> = 'RP020020'.
        <LS_TIME> = LS_TIME-ID.
        <LS_SIGNEDDATA> = ( FACTOR7 + FACTOR8  )  * FACTOR4 ."* FACTOR9.
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

        <LS_RESOURCE_ACCT> = 'AC710190'.
        <LS_TIME> = LS_TIME-ID.
        <LS_SIGNEDDATA> = ( FACTOR7 + FACTOR8   )  * FACTOR4 ."* FACTOR9.
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

"SUPER ANNUATION
    IF FACTOR2 = 1.
        <LS_RESOURCE_ACCT> = 'RP020022'.
        <LS_TIME> = LS_TIME-ID.
        <LS_SIGNEDDATA> = ( FACTOR7 + FACTOR8   )  * FACTOR5 ."* FACTOR9.
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

        <LS_RESOURCE_ACCT> = 'AC710152'. "'AC710250'.
        <LS_TIME> = LS_TIME-ID.
        <LS_SIGNEDDATA> = ( FACTOR7 + FACTOR8   )  * FACTOR5 ."* FACTOR9.
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

        ELSE.
        <LS_RESOURCE_ACCT> = 'RP020022'.
        <LS_TIME> = LS_TIME-ID.
        <LS_SIGNEDDATA> = 0.
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

        <LS_RESOURCE_ACCT> = 'AC710152'. "'AC710250'.
        <LS_TIME> = LS_TIME-ID.
        <LS_SIGNEDDATA> = 0.
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

        ENDIF.

"LONG SERVICE LEAVE
    IF FACTOR3 = 1.
        <LS_RESOURCE_ACCT> = 'RP020021'.
        <LS_TIME> = LS_TIME-ID.
        <LS_SIGNEDDATA> = ( FACTOR7 + FACTOR8   )  * FACTOR6 ."* FACTOR9.
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

        <LS_RESOURCE_ACCT> = 'AC710250'."'AC710150'.
        <LS_TIME> = LS_TIME-ID.
        <LS_SIGNEDDATA> = ( FACTOR7 + FACTOR8   )  * FACTOR6 ."* FACTOR9.
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

        ELSE.
        <LS_RESOURCE_ACCT> = 'RP020021'.
        <LS_TIME> = LS_TIME-ID.
        <LS_SIGNEDDATA> = 0 .
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

        <LS_RESOURCE_ACCT> = 'AC710250'."'AC710150'.
        <LS_TIME> = LS_TIME-ID.
        <LS_SIGNEDDATA> = 0 .
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
        ENDIF.
    ENDLOOP.
  ENDIF."//ASPAC SPECIFIC CALCULATION - END
ENDIF. "CALC IS ONLY FOR PERM EMP OR PERM ROLES

"//GEN CALC 6 -START
  CLEAR: FACTOR1, FACTOR2, FACTOR3, FACTOR4, FACTOR5, FACTOR6.
    CLEAR: LS_RESOURCE.
    READ TABLE LT_RESOURCE_FLG
    INTO LS_RESOURCE
    WITH KEY
    AUDITTRAIL = ORIG_AUDITTRAIL
    CATEGORY   = ORIG_CATEGORY
    EMPLOYEE   = ORIG_EMPLOYEE
    ENTITY     = ORIG_ENTITY
    RESOURCE_ACCT  = 'RP000425'
    RPTCURRENCY    = ORIG_CURRENCY
    TIME           = 'XXXX.INP'
    BINARY SEARCH.
    IF SY-SUBRC = 0.
      FACTOR3 = LS_RESOURCE-SIGNEDDATA.
    ELSE.
      FACTOR3 = 0.
    ENDIF.
  LOOP AT LT_TIMESCOPE INTO LS_TIME.
    CLEAR: LS_RESOURCE, FLAG1 .
    READ TABLE LT_RESOURCE_TX
    INTO LS_RESOURCE
    WITH KEY
    AUDITTRAIL = ORIG_AUDITTRAIL
    CATEGORY   = ORIG_CATEGORY
    EMPLOYEE   = ORIG_EMPLOYEE
    ENTITY     = ORIG_ENTITY
    RESOURCE_ACCT  = 'RP020001'
    RPTCURRENCY    = ORIG_CURRENCY
    TIME           = LS_TIME-ID
    BINARY SEARCH.
    IF SY-SUBRC = 0.
      FACTOR1 = LS_RESOURCE-SIGNEDDATA.
    ELSE.
      FACTOR1 = 0.
    ENDIF.

    CLEAR: LS_RESOURCE .
    READ TABLE LT_RESOURCE_TX
    INTO LS_RESOURCE
    WITH KEY
    AUDITTRAIL = ORIG_AUDITTRAIL
    CATEGORY   = ORIG_CATEGORY
    EMPLOYEE   = ORIG_EMPLOYEE
    ENTITY     = ORIG_ENTITY
    RESOURCE_ACCT  = 'RP020002'
    RPTCURRENCY    = ORIG_CURRENCY
    TIME           = LS_TIME-ID
    BINARY SEARCH.
    IF SY-SUBRC = 0.
      FACTOR2 = LS_RESOURCE-SIGNEDDATA.
    ELSE.
      FACTOR2 = 0.
    ENDIF.

    CLEAR: LS_RESOURCE .
    READ TABLE LT_RESOURCE_TX
    INTO LS_RESOURCE
    WITH KEY
    AUDITTRAIL = ORIG_AUDITTRAIL
    CATEGORY   = ORIG_CATEGORY
    EMPLOYEE   = ORIG_EMPLOYEE
    ENTITY     = ORIG_ENTITY
    RESOURCE_ACCT  = 'RP020007'
    RPTCURRENCY    = ORIG_CURRENCY
    TIME           = LS_TIME-ID
    BINARY SEARCH.
    IF SY-SUBRC = 0.
      FACTOR6 = LS_RESOURCE-SIGNEDDATA.
    ELSE.
      FACTOR6 = 0.
    ENDIF.

    <LS_TIME> = LS_TIME-ID.
    <LS_SIGNEDDATA> = FACTOR1 + FACTOR2.

      IF RESOURCE_TYPE = 'C' AND EMPLOYEE_FLAG = 'Y'."CONTRACTOR
        <LS_RESOURCE_ACCT> = 'AC710000'. "TO CLEAR DATA AS PER LUKE'S TEST
        <LS_SIGNEDDATA> = 0.
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
        "<LS_RESOURCE_ACCT> = 'AC780000'. CR2586 USE AC710110 INSTEAD OF AC780000
        <LS_RESOURCE_ACCT> = 'AC710110'.  "CR2586 USE AC710110 INSTEAD OF AC780000
        <LS_SIGNEDDATA> = FACTOR1 + FACTOR2.
        FLAG1 = 1.
      ENDIF.

      IF RESOURCE_TYPE = 'E' AND EMPLOYEE_FLAG = 'Y'."PERMANENT
        "<LS_RESOURCE_ACCT> = 'AC780000'. CR2586 USE AC710110 INSTEAD OF AC780000
        <LS_RESOURCE_ACCT> = 'AC710110'.  "CR2586 USE AC710110 INSTEAD OF AC780000
        <LS_SIGNEDDATA> = 0.
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
        <LS_SIGNEDDATA> = ( FACTOR1 + FACTOR2 ) . " - FACTOR6. NEW_002
        <LS_RESOURCE_ACCT> = 'AC710000'.
        FLAG1 = 1.
      ENDIF.

      READ TABLE LT_TOTAL_ROLES TRANSPORTING NO FIELDS
      WITH KEY TABLE_LINE = ORIG_EMPLOYEE.
      IF SY-SUBRC = 0 AND FACTOR3 = 1.
        <LS_RESOURCE_ACCT> = 'AC710000'. "TO CLEAR DATA AS PER LUKE'S TEST
        <LS_SIGNEDDATA> = 0.
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

        <LS_SIGNEDDATA> = FACTOR1 + FACTOR2.
        "<LS_RESOURCE_ACCT> = 'AC780000'. CR2586 USE AC710110 INSTEAD OF AC780000
        <LS_RESOURCE_ACCT> = 'AC710110'.  "CR2586 USE AC710110 INSTEAD OF AC780000
        FLAG1 = 1.
      ELSEIF SY-SUBRC = 0 AND FACTOR3 <> 1.
        "<LS_RESOURCE_ACCT> = 'AC780000'. CR2586 USE AC710110 INSTEAD OF AC780000
        <LS_RESOURCE_ACCT> = 'AC710110'.  "CR2586 USE AC710110 INSTEAD OF AC780000
        <LS_SIGNEDDATA> = 0.
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
        <LS_SIGNEDDATA> = ( FACTOR1 + FACTOR2 ) . " - FACTOR6. NEW_002
        <LS_RESOURCE_ACCT> = 'AC710000'. "PERM ROLE
        FLAG1 = 1.
        ENDIF.

      IF FLAG1 = 1.
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
       "BELOW UPDATING THE LT_RESOURCE_TX START
        CLEAR LS_RESOURCE.
        LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
        LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
        LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
        LS_RESOURCE-ENTITY      = <LS_ENTITY>.
        LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
        LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
        LS_RESOURCE-TIME          = <LS_TIME>.
        LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
        LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
          AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
          AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
          AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
          AND RPTCURRENCY = <LS_RPTCURRENCY> AND
          TIME = <LS_TIME>.
          <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
          ENDLOOP.
          IF SY-SUBRC <> 0.
            INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
            ENDIF.
        "ABOVE UPDATING THE LT_RESOURCE_TX END
        ENDIF.
        CLEAR FLAG1.
  ENDLOOP.
"//GEN CALC 6 -END

"//GEN CALC 7 AND 8 START
CLEAR: FACTOR1, FACTOR2, FACTOR3, FACTOR4, FACTOR5, FACTOR6, FACTOR7.
IF REGION = 'UK'.
LOOP AT LT_TIMESCOPE INTO LS_TIME.
    CLEAR: LS_RESOURCE.
    READ TABLE LT_RESOURCE_TX
    INTO LS_RESOURCE
    WITH KEY
    AUDITTRAIL = ORIG_AUDITTRAIL
    CATEGORY   = ORIG_CATEGORY
    EMPLOYEE   = ORIG_EMPLOYEE
    ENTITY     = ORIG_ENTITY
    RESOURCE_ACCT  = 'AC710110'  "CR2586 USE AC710110 INSTEAD OF AC780000
    RPTCURRENCY    = ORIG_CURRENCY
    TIME           = LS_TIME-ID
    BINARY SEARCH.
    IF SY-SUBRC = 0.
      FACTOR1 = LS_RESOURCE-SIGNEDDATA.
    ELSE.
      FACTOR1 = 0.
    ENDIF.

    CLEAR: LS_RESOURCE.
    READ TABLE LT_RESOURCE_TX
    INTO LS_RESOURCE
    WITH KEY
    AUDITTRAIL = ORIG_AUDITTRAIL
    CATEGORY   = ORIG_CATEGORY
    EMPLOYEE   = ORIG_EMPLOYEE
    ENTITY     = ORIG_ENTITY
    RESOURCE_ACCT  = 'AC740000'
    RPTCURRENCY    = ORIG_CURRENCY
    TIME           = LS_TIME-ID
    BINARY SEARCH.
    IF SY-SUBRC = 0.
      FACTOR2 = LS_RESOURCE-SIGNEDDATA.
    ELSE.
      FACTOR2 = 0.
    ENDIF.

    CLEAR: LS_RESOURCE.
    READ TABLE LT_RESOURCE_TX
    INTO LS_RESOURCE
    WITH KEY
    AUDITTRAIL = ORIG_AUDITTRAIL
    CATEGORY   = ORIG_CATEGORY
    EMPLOYEE   = ORIG_EMPLOYEE
    ENTITY     = ORIG_ENTITY
    RESOURCE_ACCT  = 'AC710355' "'AC700006'
    RPTCURRENCY    = ORIG_CURRENCY
    TIME           = LS_TIME-ID
    BINARY SEARCH.
    IF SY-SUBRC = 0.
      FACTOR3 = LS_RESOURCE-SIGNEDDATA.
    ELSE.
      FACTOR3 = 0.
    ENDIF.

    CLEAR: LS_RESOURCE.
    READ TABLE LT_RESOURCE_TX
    INTO LS_RESOURCE
    WITH KEY
    AUDITTRAIL = ORIG_AUDITTRAIL
    CATEGORY   = ORIG_CATEGORY
    EMPLOYEE   = ORIG_EMPLOYEE
    ENTITY     = ORIG_ENTITY
    RESOURCE_ACCT  = 'AC700007'
    RPTCURRENCY    = ORIG_CURRENCY
    TIME           = LS_TIME-ID
    BINARY SEARCH.
    IF SY-SUBRC = 0.
      FACTOR4 = LS_RESOURCE-SIGNEDDATA.
    ELSE.
      FACTOR4 = 0.
    ENDIF.


"GEN CALC 7
*****    IF COMPANY = 'S101'."REMOVE S101 SEP21
*****      <LS_RESOURCE_ACCT> = 'AC310000P'.
*****      <LS_TIME> = LS_TIME-ID.
*****      <LS_SIGNEDDATA> = ( FACTOR1 + FACTOR2 ) * VAR_ITAX.
*****      FACTOR5 = <LS_SIGNEDDATA>.
*****      COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
*****
*****      <LS_RESOURCE_ACCT> = 'AC300000P'.
*****      COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
*****
*****      ELSE.
      <LS_RESOURCE_ACCT> = 'AC310000P'.
      <LS_TIME> = LS_TIME-ID.
      "<LS_SIGNEDDATA> = ( FACTOR1 + FACTOR2 +  FACTOR3 ) * VAR_ITAX. "Change 4 27.09.2017 VZ - REMOVE LINES
      <LS_SIGNEDDATA> = ( FACTOR1 + FACTOR2 ) * VAR_ITAX. "Change 4 27.09.2017 VZ - ADD LINES
      FACTOR5 = <LS_SIGNEDDATA>.
      COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

      <LS_RESOURCE_ACCT> = 'AC300000P'.
      COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.


*****      ENDIF."REMOVE S101 SEP21

  ENDLOOP.
  ENDIF.
"//GEN CALC 7 AND 8 END

"//GEN CALC 9 AND 10 START
CLEAR: FACTOR1, FACTOR2, FACTOR3, FACTOR4, FACTOR5, FACTOR6, FACTOR7.
IF  REGION = 'ME' OR  REGION = 'EU' OR REGION = 'ASPAC'.
LOOP AT LT_TIMESCOPE INTO LS_TIME.

    CLEAR: LS_RESOURCE.
    READ TABLE LT_RESOURCE_TX
    INTO LS_RESOURCE
    WITH KEY
    AUDITTRAIL = ORIG_AUDITTRAIL
    CATEGORY   = ORIG_CATEGORY
    EMPLOYEE   = ORIG_EMPLOYEE
    ENTITY     = ORIG_ENTITY
    RESOURCE_ACCT  = 'AC710110'  "CR2586 USE AC710110 INSTEAD OF AC780000
    RPTCURRENCY    = ORIG_CURRENCY
    TIME           = LS_TIME-ID
    BINARY SEARCH.
    IF SY-SUBRC = 0.
      FACTOR1 = LS_RESOURCE-SIGNEDDATA.
    ELSE.
      FACTOR1 = 0.
    ENDIF.

    CLEAR: LS_RESOURCE.
    READ TABLE LT_RESOURCE_TX
    INTO LS_RESOURCE
    WITH KEY
    AUDITTRAIL = ORIG_AUDITTRAIL
    CATEGORY   = ORIG_CATEGORY
    EMPLOYEE   = ORIG_EMPLOYEE
    ENTITY     = ORIG_ENTITY
    RESOURCE_ACCT  = 'AC740000'
    RPTCURRENCY    = ORIG_CURRENCY
    TIME           = LS_TIME-ID
    BINARY SEARCH.
    IF SY-SUBRC = 0.
      FACTOR2 = LS_RESOURCE-SIGNEDDATA.
    ELSE.
      FACTOR2 = 0.
    ENDIF.

    CLEAR: LS_RESOURCE.
    READ TABLE LT_RESOURCE_TX
    INTO LS_RESOURCE
    WITH KEY
    AUDITTRAIL = ORIG_AUDITTRAIL
    CATEGORY   = ORIG_CATEGORY
    EMPLOYEE   = ORIG_EMPLOYEE
    ENTITY     = ORIG_ENTITY
    RESOURCE_ACCT  = 'AC710355'     "'AC700006'
    RPTCURRENCY    = ORIG_CURRENCY
    TIME           = LS_TIME-ID
    BINARY SEARCH.
    IF SY-SUBRC = 0.
      FACTOR3 = LS_RESOURCE-SIGNEDDATA.
    ELSE.
      FACTOR3 = 0.
    ENDIF.

    CLEAR: LS_RESOURCE.
    READ TABLE LT_RESOURCE_TX
    INTO LS_RESOURCE
    WITH KEY
    AUDITTRAIL = ORIG_AUDITTRAIL
    CATEGORY   = ORIG_CATEGORY
    EMPLOYEE   = ORIG_EMPLOYEE
    ENTITY     = ORIG_ENTITY
    RESOURCE_ACCT  = 'AC700007'
    RPTCURRENCY    = ORIG_CURRENCY
    TIME           = LS_TIME-ID
    BINARY SEARCH.
    IF SY-SUBRC = 0.
      FACTOR4 = LS_RESOURCE-SIGNEDDATA.
    ELSE.
      FACTOR4 = 0.
    ENDIF.

"GEN CALC 9

      <LS_RESOURCE_ACCT> = 'AC310000P'.
      <LS_TIME> = LS_TIME-ID.
      <LS_SIGNEDDATA> = ( FACTOR1 ) * VAR_ITAX_ROW.
      FACTOR5 = <LS_SIGNEDDATA>.
      COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

      "CR 2586 CHANGE
      <LS_RESOURCE_ACCT> = 'AC300000P'.
      COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
      "CR 2586 CHANGE

ENDLOOP.

ENDIF.
"//GEN CALC 9 AND 10 END

  WHEN 'ITS_C'.  "'ITS_CREDIT'. "ITSS DEMAND COST RECOVERY
    "CHECK IF THE EMPLOYEE IS A CHILD OF ITSSROLES
    IF ITSS_FLAG = 'Y' AND REGION = 'UK'.

      "CLEAR DATA ON AUDITTRAIL = INPUT_ITSCREDIT
      LOOP AT LT_TIMESCOPE INTO LS_TIME.
        <LS_SIGNEDDATA>     = 0 .
        <LS_RESOURCE_ACCT>  = 'AC710355' .
        <LS_TIME>           = LS_TIME-ID.
        <LS_ENTITY>         = ITSS_CC.
        <LS_AUDITTRAIL>     = 'INPUT_ITSCREDIT'. "CG CHANGE MAY 11
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

****        IF ( REGION = 'UK' ) AND COMPANY = 'S101'.  "REMOVE S101 SEP21
****            <LS_SIGNEDDATA> = 0 .
****            <LS_RESOURCE_ACCT> = 'AC220000' .
****            <LS_TIME> = LS_TIME-ID.
****            <LS_ENTITY> = ITSS_CC.
****            <LS_AUDITTRAIL>     = 'INPUT_ITSCREDIT'. "CG CHANGE MAY 11
****            COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
****            ENDIF.
        ENDLOOP.

    CLEAR: FACTOR1, FACTOR2, FACTOR3, FACTOR4, FACTOR5, FACTOR6, FACTOR7, FACTOR8.

      LOOP AT LT_TIMESCOPE INTO LS_TIME.

        CLEAR: LS_RESOURCE.
        READ TABLE LT_RESOURCE_TX
        INTO LS_RESOURCE
        WITH KEY
        AUDITTRAIL = ORIG_AUDITTRAIL
        CATEGORY   = ORIG_CATEGORY
        EMPLOYEE   = ORIG_EMPLOYEE
        ENTITY     = ORIG_ENTITY
        RESOURCE_ACCT  = 'RP000431'
        RPTCURRENCY    = ORIG_CURRENCY
        TIME           = LS_TIME-ID
        BINARY SEARCH.
        IF SY-SUBRC = 0.
          FACTOR1 = LS_RESOURCE-SIGNEDDATA.
        ELSE.
          FACTOR1 = 0.
        ENDIF.

        READ TABLE LT_RESOURCE_TX
        INTO LS_RESOURCE
        WITH KEY
        AUDITTRAIL = ORIG_AUDITTRAIL
        CATEGORY   = ORIG_CATEGORY
        EMPLOYEE   = ORIG_EMPLOYEE
        ENTITY     = ORIG_ENTITY
        RESOURCE_ACCT  = 'AC213000'
        RPTCURRENCY    = ORIG_CURRENCY
        TIME           = LS_TIME-ID
        BINARY SEARCH.
        IF SY-SUBRC = 0.
          FACTOR6 = LS_RESOURCE-SIGNEDDATA.
        ELSE.
          FACTOR6 = 0.
        ENDIF.

        IF REGION = 'UK'.
          CLEAR LS_RESOURCE.
          READ TABLE LT_RESOURCE_FLG
          INTO LS_RESOURCE
          WITH KEY
          AUDITTRAIL    = 'INPUT'
          CATEGORY      = WORKING_CATEGORY
          EMPLOYEE      = WA_MEMBER
          ENTITY        = 'NO_PROFITCENTREUK'
          RESOURCE_ACCT  = 'RP000430'
          RPTCURRENCY    = 'LC'
          TIME           = YEAR_INP
          BINARY SEARCH.
          IF SY-SUBRC = 0 AND LS_RESOURCE-SIGNEDDATA <> 0 .
            VAR_ITSS_RATES = LS_RESOURCE-SIGNEDDATA.
            ELSE.
            VAR_ITSS_RATES = 0 .
            ENDIF.

          ELSE.

VAR_ITSS_RATES_ROW = 0 . "BECAUSE ROW IS OUT OF SCOPE

          ENDIF.

        READ TABLE LT_RESOURCE_TX
        INTO LS_RESOURCE
        WITH KEY
        AUDITTRAIL = ORIG_AUDITTRAIL
        CATEGORY   = ORIG_CATEGORY
        EMPLOYEE   = ORIG_EMPLOYEE
        ENTITY     = ORIG_ENTITY
        RESOURCE_ACCT  = 'AC710355'
        RPTCURRENCY    = ORIG_CURRENCY
        TIME           = LS_TIME-ID
        BINARY SEARCH.
        IF SY-SUBRC = 0.
          FACTOR8 = LS_RESOURCE-SIGNEDDATA.
        ELSE.
          FACTOR8 = 0.
        ENDIF.

IF REGION = 'UK'.
  FACTOR2 = VAR_ITSS_RATES.
  FACTOR3 = VAR_ITAX.
  ELSE.
  FACTOR2 = VAR_ITSS_RATES_ROW.
  FACTOR3 = VAR_ITAX_ROW.
    ENDIF.

"//CREATE ITS CREDIT IN ITS COST CENTRES
    <LS_SIGNEDDATA>     = ( FACTOR8 ) * -1 .
    <LS_RESOURCE_ACCT>  = 'AC710355' .
    <LS_TIME>           = LS_TIME-ID.
    <LS_ENTITY>         = ITSS_CC.
    <LS_AUDITTRAIL>     = 'INPUT_ITSCREDIT'. "CG CHANGE MAY 11
    COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
    <LS_ENTITY>     = ORIG_ENTITY.
    <LS_AUDITTRAIL> = ORIG_AUDITTRAIL.

      """"IF ( REGION = 'UK' ) AND COMPANY = 'S101'.
        "//CALCULATE IC CASH FOR INTRA COMPANY ITS DEMAND
          <LS_SIGNEDDATA> = FACTOR8 ."( FACTOR1 * FACTOR2 ) .
          <LS_RESOURCE_ACCT> = 'AC220000' .
          <LS_TIME> = LS_TIME-ID.
          <LS_ENTITY> = ITSS_CC.
          <LS_AUDITTRAIL>     = 'INPUT_ITSCREDIT'. "CG CHANGE MAY 11
          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
          <LS_ENTITY>     = ORIG_ENTITY.
          <LS_AUDITTRAIL> = ORIG_AUDITTRAIL.
          """"ENDIF.

      ENDLOOP.

ENDIF. "ITSS_FLAG


  WHEN 'OVER1'. "OVERNIGHT CALCULATION 1

    CLEAR: FACTOR1 , FACTOR2 .
    CLEAR: LS_RESOURCE.
    READ TABLE LT_RESOURCE_FLG
    INTO LS_RESOURCE
    WITH KEY
    AUDITTRAIL    = ORIG_AUDITTRAIL
    CATEGORY      = ORIG_CATEGORY
    EMPLOYEE      = ORIG_EMPLOYEE
    ENTITY        = ORIG_ENTITY
    RESOURCE_ACCT  = 'RP010001' "FACTOR2
    RPTCURRENCY    = ORIG_CURRENCY
    TIME           = YEAR_INP
    BINARY SEARCH.
    IF SY-SUBRC = 0 .
      FACTOR2 = LS_RESOURCE-SIGNEDDATA .
      ELSE.
        FACTOR2 = 0.
        ENDIF.

   IF FACTOR2 = 0 . "COPY OVR IF TARGET IS BLANK
    "COPY SALARY AMOUNT TO CURRENT YEAR INP
    CLEAR: LS_RESOURCE.
    READ TABLE LT_RESOURCE_FLG
    INTO LS_RESOURCE
    WITH KEY
    AUDITTRAIL    = ORIG_AUDITTRAIL
    CATEGORY      = ORIG_CATEGORY
    EMPLOYEE      = ORIG_EMPLOYEE
    ENTITY        = ORIG_ENTITY
    RESOURCE_ACCT  = 'RP010001' "FACTOR1
    RPTCURRENCY    = ORIG_CURRENCY
    TIME           = 'XXXX.INP'
    BINARY SEARCH.
    IF SY-SUBRC = 0 AND LS_RESOURCE-SIGNEDDATA IS NOT INITIAL.
      FACTOR1 = LS_RESOURCE-SIGNEDDATA .
      <LS_TIME> = YEAR_INP.
      COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
      ELSE.
        FACTOR1 = 0.
        ENDIF.
     ENDIF.


  WHEN 'ITS_R'. "FOR RATE CARD

"//CALCULATE ITSS COST TO CONTRACT
****"ONLY FOR EMPLOYEES IN LT_ITSSROLES
 IF ITSS_FLAG = 'Y' AND REGION = 'UK'."EMPLOYEE IS UNDER TOTAL_ITSSROLES

  "BLANK CLEARING OF TOTAL_STSTACCT AND TOTAL_MONTHLYVALUES AS THESE WILL BE RE CALCULATED FURTHER DOWN THE LINE.

  LOOP AT LT_TIMESCOPE INTO LS_TIME.

    <LS_SIGNEDDATA> = 0.
    <LS_TIME> = LS_TIME-ID .
    LOOP AT LT_TOTAL_STATACCT INTO WA_MEMBER.
    <LS_RESOURCE_ACCT> = WA_MEMBER.
    COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
    ENDLOOP.

    LOOP AT LT_TOTAL_MONTHLYVALUES INTO WA_MEMBER.
    <LS_RESOURCE_ACCT> = WA_MEMBER.
    COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
    ENDLOOP.

    ENDLOOP.
    <LS_SIGNEDDATA>     = VAR_ORIGAMOUNT.
    <LS_RESOURCE_ACCT>  = ORIG_ACCOUNT .
    <LS_TIME>           = ORIG_TIME .


   LOOP AT LT_TIMESCOPE INTO LS_TIME.

    CLEAR LS_RESOURCE.
    READ TABLE LT_RESOURCE_TX
    INTO LS_RESOURCE
    WITH KEY
    AUDITTRAIL    = ORIG_AUDITTRAIL
    CATEGORY      = ORIG_CATEGORY
    EMPLOYEE      = ORIG_EMPLOYEE
    ENTITY        = ORIG_ENTITY
    RESOURCE_ACCT  = 'RP000431' "FACTOR14
    RPTCURRENCY    = ORIG_CURRENCY
    TIME           = LS_TIME-ID
    BINARY SEARCH.
    IF SY-SUBRC = 0 .
      FACTOR14 = LS_RESOURCE-SIGNEDDATA.
      ELSE.
        FACTOR14 = 0.
      ENDIF.

      CLEAR LS_RESOURCE.
      READ TABLE LT_RESOURCE_FLG
      INTO LS_RESOURCE
      WITH KEY
      AUDITTRAIL    = 'INPUT'
      CATEGORY      = WORKING_CATEGORY
      EMPLOYEE      = ORIG_EMPLOYEE
      ENTITY        = 'NO_PROFITCENTREUK'
      RESOURCE_ACCT  = 'RP000430'
      RPTCURRENCY    = 'LC'
      TIME           = YEAR_INP
      BINARY SEARCH.
      IF SY-SUBRC = 0 .
        VAR_ITSS_RATES = LS_RESOURCE-SIGNEDDATA.
        ELSE.
          VAR_ITSS_RATES = 0.
        ENDIF.

    <LS_SIGNEDDATA> = VAR_ITSS_RATES * FACTOR14 .
    FACTOR16 = <LS_SIGNEDDATA>.

******  IF COMPANY = 'S101'. "REMOVE S101 SEP21
******        <LS_RESOURCE_ACCT> = 'AC710355'.  "'AC700006' .
******        <LS_TIME> = LS_TIME-ID .
******        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
******            "BELOW UPDATING THE LT_RESOURCE_TX START
******            CLEAR LS_RESOURCE.
******            LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
******            LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
******            LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
******            LS_RESOURCE-ENTITY      = <LS_ENTITY>.
******            LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
******            LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
******            LS_RESOURCE-TIME          = <LS_TIME>.
******            LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
******            LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
******              AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
******              AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
******              AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
******              AND RPTCURRENCY = <LS_RPTCURRENCY> AND
******              TIME = <LS_TIME>.
******              <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
******              ENDLOOP.
******              IF SY-SUBRC <> 0.
******                INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
******                ENDIF.
******            "ABOVE UPDATING THE LT_RESOURCE_TX END
******
******        <LS_RESOURCE_ACCT> = 'RP020400' .
******        <LS_TIME> = LS_TIME-ID .
******        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
******
******        <LS_RESOURCE_ACCT> = 'AC220000' .
******        <LS_TIME> = LS_TIME-ID .
******        <LS_SIGNEDDATA> = FACTOR16 * -1 .
******        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
******        <LS_SIGNEDDATA> = FACTOR16 .
******
******    ELSE.
        <LS_RESOURCE_ACCT> = 'AC710355'.  "'AC700007' .
        <LS_TIME> = LS_TIME-ID .
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
            "BELOW UPDATING THE LT_RESOURCE_TX START
            CLEAR LS_RESOURCE.
            LS_RESOURCE-AUDITTRAIL  = <LS_AUDITTRAIL>.
            LS_RESOURCE-CATEGORY    = <LS_CATEGORY>.
            LS_RESOURCE-EMPLOYEE    = <LS_EMPLOYEE>.
            LS_RESOURCE-ENTITY      = <LS_ENTITY>.
            LS_RESOURCE-RESOURCE_ACCT = <LS_RESOURCE_ACCT>.
            LS_RESOURCE-RPTCURRENCY   = <LS_RPTCURRENCY>.
            LS_RESOURCE-TIME          = <LS_TIME>.
            LS_RESOURCE-SIGNEDDATA    = <LS_SIGNEDDATA>.
            LOOP AT LT_RESOURCE_TX ASSIGNING <LS_RESOURCE> WHERE
              AUDITTRAIL = <LS_AUDITTRAIL> AND CATEGORY = <LS_CATEGORY>
              AND EMPLOYEE = <LS_EMPLOYEE> AND ENTITY = <LS_ENTITY>
              AND RESOURCE_ACCT = <LS_RESOURCE_ACCT>
              AND RPTCURRENCY = <LS_RPTCURRENCY> AND
              TIME = <LS_TIME>.
              <LS_RESOURCE>-SIGNEDDATA = <LS_SIGNEDDATA>.
              ENDLOOP.
              IF SY-SUBRC <> 0.
                INSERT LS_RESOURCE INTO TABLE LT_RESOURCE_TX.
                ENDIF.
            "ABOVE UPDATING THE LT_RESOURCE_TX END

        <LS_RESOURCE_ACCT> = 'RP020400' .
        <LS_TIME> = LS_TIME-ID .
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

****        <LS_RESOURCE_ACCT> = 'AC213010' . "change3
****        <LS_TIME> = LS_TIME-ID .          "change3
****        <LS_SIGNEDDATA> = FACTOR16 * -1 .  "change3
****        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.  "change3
        <LS_SIGNEDDATA> = FACTOR16 .

*****    ENDIF."REMOVE S101 SEP21

    TIME_STRING = <LS_TIME>.
    TIME_STRING = TIME_STRING(4).

    IF TIME_STRING = CATEGORY_YEAR  AND WORKING_CATEGORY = 'WFORECAST'."ITSSCOSTS_UK

        <LS_RESOURCE_ACCT> = 'RP020400' .
        <LS_CATEGORY> = 'CY_FCST'.
        <LS_TIME> = 'XXXX.INP' .
        <LS_SIGNEDDATA> = FACTOR16 . "RP000430 * RP000431 FOR FIRST YEAR
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

        <LS_RESOURCE_ACCT> = 'RP000431' .
        <LS_CATEGORY> = 'CY_FCST'.
        <LS_TIME> = 'XXXX.INP' .
        <LS_SIGNEDDATA> = FACTOR14 ."SUM OF RP000431
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
      ENDIF.


  ENDLOOP.
  ENDIF. "ITSS_FLAG ='Y' AND REGION = UK

  WHEN 'DRVRS'.
*************************************************************************************************
"PUT CALC HERE TO PROCESS THE ADMIN DRIVERS
"OTH_UK 1 OTH_ME 1 OTH_ASPAC 1
  IF ( ( RESOURCE_TYPE = 'E' AND EMPLOYEE_FLAG = 'Y' )  OR ( VAR_RP000425 = 0 AND ROLES_FLAG = 'Y' ) )
    AND LT_TARGET_ACCT IS NOT INITIAL AND ( REGION = 'UK' OR REGION = 'ME' OR REGION = 'ASPAC').  "CALC IS ONLY FOR PERM EMP OR PERM ROLES

    CLEAR: FACTOR1, FACTOR2,FACTOR3, FACTOR4,FACTOR5, FACTOR6,FACTOR7, FACTOR8.

    CLEAR LS_RESOURCE.
    READ TABLE LT_RESOURCE_FLG
    INTO LS_RESOURCE
    WITH KEY
    AUDITTRAIL = ORIG_AUDITTRAIL
    CATEGORY   = ORIG_CATEGORY
    EMPLOYEE   = ORIG_EMPLOYEE
    ENTITY     = ORIG_ENTITY
    RESOURCE_ACCT  = 'RP000400'
    RPTCURRENCY    = ORIG_CURRENCY
    TIME           = YEAR_INP
    BINARY SEARCH.
    IF SY-SUBRC = 0.
      FACTOR1 = LS_RESOURCE-SIGNEDDATA.
    ELSE.
      FACTOR1 = 0.
    ENDIF.

    READ TABLE LT_TARGET_ACCT INTO WA_MEMBER
    WITH KEY TABLE_LINE = 'RP000401'
    BINARY SEARCH.
    IF SY-SUBRC = 0.
      FACTOR2 = 1.
    ELSE.
      FACTOR2 = 0.
    ENDIF.

    READ TABLE LT_TARGET_ACCT INTO WA_MEMBER
    WITH KEY TABLE_LINE = 'RP900001'
    BINARY SEARCH.
    IF SY-SUBRC = 0.
      FACTOR3 = 1.
    ELSE.
      FACTOR3 = 0.
    ENDIF.


    IF FACTOR1 <> 1 AND FACTOR2 = 1 AND FACTOR3 = 1. "'RP000400'
      CLEAR LS_RESOURCE.
      READ TABLE LT_RESOURCE_FLG
      INTO LS_RESOURCE
      WITH KEY
      AUDITTRAIL = ORIG_AUDITTRAIL
      CATEGORY   = ORIG_CATEGORY
      EMPLOYEE   = 'NO_EMPLOYEE'
      ENTITY     = ENTITY_DRIVER "'NO_PROFITCENTREUK'
      RESOURCE_ACCT  = 'RP000401'
      RPTCURRENCY    = ORIG_CURRENCY
      TIME           = YEAR_INP
      BINARY SEARCH.
      IF SY-SUBRC = 0.
        <LS_SIGNEDDATA>     = LS_RESOURCE-SIGNEDDATA.
        <LS_RESOURCE_ACCT>  = 'RP000401' .
        <LS_TIME>           = YEAR_INP.
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

      ENDIF.

      CLEAR LS_RESOURCE.
      READ TABLE LT_RESOURCE_FLG
      INTO LS_RESOURCE
      WITH KEY
      AUDITTRAIL = ORIG_AUDITTRAIL
      CATEGORY   = ORIG_CATEGORY
      EMPLOYEE   = 'NO_EMPLOYEE'
      ENTITY     = ENTITY_DRIVER "'NO_PROFITCENTREUK'
      RESOURCE_ACCT  = 'RP900001'
      RPTCURRENCY    = ORIG_CURRENCY
      TIME           = YEAR_INP
      BINARY SEARCH.
      IF SY-SUBRC = 0.
        <LS_SIGNEDDATA>     = LS_RESOURCE-SIGNEDDATA.
        <LS_RESOURCE_ACCT>  = 'RP900001' .
        <LS_TIME>           = YEAR_INP.
        COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
      ENDIF.
      ENDIF.
    ENDIF.

"OTH_UK 7 AND OTH_UK 8
  IF ( ( RESOURCE_TYPE = 'E' AND EMPLOYEE_FLAG = 'Y' )  OR ( VAR_RP000425 = 0 AND ROLES_FLAG = 'Y' ) )
    AND LT_TARGET_ACCT IS NOT INITIAL AND ( REGION = 'UK' ).  "CALC IS ONLY FOR PERM EMP OR PERM ROLES

    CLEAR: FACTOR1, FACTOR2,FACTOR3, FACTOR4,FACTOR5, FACTOR6,FACTOR7, FACTOR8.

    READ TABLE LT_TARGET_ACCT INTO WA_MEMBER
    WITH KEY TABLE_LINE = 'RP900006'
    BINARY SEARCH.
    IF SY-SUBRC = 0.
      READ TABLE LT_RESOURCE_FLG
      INTO LS_RESOURCE
      WITH KEY
      AUDITTRAIL = ORIG_AUDITTRAIL
      CATEGORY   = ORIG_CATEGORY
      EMPLOYEE   = ORIG_EMPLOYEE
      ENTITY     = ORIG_ENTITY
      RESOURCE_ACCT  = 'RP900006'
      RPTCURRENCY    = ORIG_CURRENCY
      TIME           = 'XXXX.INP'
      BINARY SEARCH.
      IF SY-SUBRC = 0.
        FACTOR1 = LS_RESOURCE-SIGNEDDATA.
      ELSE.
        FACTOR1 = 0.
      ENDIF.

      CASE FACTOR1.

        WHEN 10 .
          ACCOUNT_DRIVER = 'RP000010'.
        WHEN 11 .
          ACCOUNT_DRIVER = 'RP000011'.
        WHEN 12 .
          ACCOUNT_DRIVER = 'RP000012'.
        WHEN 13 .
          ACCOUNT_DRIVER = 'RP000013'.
        WHEN 14 .
          ACCOUNT_DRIVER = 'RP000014'.
        WHEN OTHERS.
          ACCOUNT_DRIVER = 'DONOTHING'.
        ENDCASE.

        CLEAR LS_RESOURCE.
        READ TABLE LT_RESOURCE_FLG
        INTO LS_RESOURCE
        WITH KEY
        AUDITTRAIL = ORIG_AUDITTRAIL
        CATEGORY   = ORIG_CATEGORY
        EMPLOYEE   = 'NO_EMPLOYEE'
        ENTITY     = ENTITY_DRIVER
        RESOURCE_ACCT  =  ACCOUNT_DRIVER
        RPTCURRENCY    = ORIG_CURRENCY
        TIME           = YEAR_INP
        BINARY SEARCH.
        IF SY-SUBRC = 0.
          <LS_SIGNEDDATA>     = LS_RESOURCE-SIGNEDDATA.
          <LS_RESOURCE_ACCT>  = 'RP010008' .
          <LS_TIME>           = YEAR_INP.
          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.
          ENDIF.
    ENDIF.

    READ TABLE LT_TARGET_ACCT INTO WA_MEMBER
    WITH KEY TABLE_LINE = 'RP900005'
    BINARY SEARCH.
    IF SY-SUBRC = 0.
      READ TABLE LT_RESOURCE_FLG
      INTO LS_RESOURCE
      WITH KEY
      AUDITTRAIL = ORIG_AUDITTRAIL
      CATEGORY   = ORIG_CATEGORY
      EMPLOYEE   = ORIG_EMPLOYEE
      ENTITY     = ORIG_ENTITY
      RESOURCE_ACCT  = 'RP900005'
      RPTCURRENCY    = ORIG_CURRENCY
      TIME           = 'XXXX.INP'
      BINARY SEARCH.
      IF SY-SUBRC = 0.
        FACTOR1 = LS_RESOURCE-SIGNEDDATA.
      ELSE.
        FACTOR1 = 0.

      ENDIF.

      READ TABLE LT_RESOURCE_FLG
      INTO LS_RESOURCE
      WITH KEY
      AUDITTRAIL = ORIG_AUDITTRAIL
      CATEGORY   = ORIG_CATEGORY
      EMPLOYEE   = ORIG_EMPLOYEE
      ENTITY     = ORIG_ENTITY
      RESOURCE_ACCT  = 'RP000027'
      RPTCURRENCY    = ORIG_CURRENCY
      TIME           = 'XXXX.INP'
      BINARY SEARCH.
      IF SY-SUBRC = 0.
        FACTOR2 = LS_RESOURCE-SIGNEDDATA.
        IF FACTOR2 = 1 .
          WA_MEMBER   = 'RP000020'.
          WA_MEMBERX  = 'RP000021'.
          ELSE.
            WA_MEMBER   = 'RP000021'.
            WA_MEMBERX  = 'RP000020'.
            ENDIF.
      ELSE.
        FACTOR2 = 0.
        WA_MEMBER = 'NOACCOUNT'.
      ENDIF.

      CASE FACTOR1.

        WHEN 10 .
          ACCOUNT_DRIVER = 'RP000015'.
        WHEN 11 .
          ACCOUNT_DRIVER = 'RP000016'.
        WHEN 12 .
          ACCOUNT_DRIVER = 'RP000017'.
        WHEN 13 .
          ACCOUNT_DRIVER = 'RP000018'.
        WHEN 14 .
          ACCOUNT_DRIVER = 'RP000019'.
        WHEN OTHERS.
          ACCOUNT_DRIVER = 'DONOTHING'.
        ENDCASE.

        CLEAR LS_RESOURCE.
        READ TABLE LT_RESOURCE_FLG
        INTO LS_RESOURCE
        WITH KEY
        AUDITTRAIL = ORIG_AUDITTRAIL
        CATEGORY   = ORIG_CATEGORY
        EMPLOYEE   = 'NO_EMPLOYEE'
        ENTITY     = ENTITY_DRIVER
        RESOURCE_ACCT  =  ACCOUNT_DRIVER
        RPTCURRENCY    = ORIG_CURRENCY
        TIME           = YEAR_INP
        BINARY SEARCH.
        IF SY-SUBRC = 0  AND WA_MEMBER <> 'NOACCOUNT' .
          <LS_SIGNEDDATA>     = LS_RESOURCE-SIGNEDDATA.
          <LS_RESOURCE_ACCT>  = WA_MEMBER . "'RP000020' .
          <LS_TIME>           = YEAR_INP.
          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

          <LS_SIGNEDDATA>     = 0.
          <LS_RESOURCE_ACCT>  = WA_MEMBERX .
          <LS_TIME>           = YEAR_INP.
          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

          ENDIF.

    ENDIF.
    ENDIF. "OTH_UK 7 AND OTH_UK 8 END

"OTH_ME 4
**  IF ( ( RESOURCE_TYPE = 'E' AND EMPLOYEE_FLAG = 'Y' )  OR ( VAR_RP000425 = 0 AND ROLES_FLAG = 'Y' ) )
**    AND LT_TARGET_ACCT IS NOT INITIAL AND ( REGION = 'ME' OR REGION = 'ASPAC' ).  "CALC IS ONLY FOR PERM EMP OR PERM ROLES
  IF LT_TARGET_ACCT IS NOT INITIAL AND ( REGION = 'ME' ) AND GATE1 = 1 AND GATE2 = 1.  "CALC IS ONLY FOR PERM EMP OR PERM ROLES


    CLEAR: FACTOR1, FACTOR2,FACTOR3, FACTOR4,FACTOR5, FACTOR6,FACTOR7, FACTOR8.

    READ TABLE LT_TARGET_ACCT INTO WA_MEMBER
    WITH KEY TABLE_LINE = 'RP900006'
    BINARY SEARCH.
    IF SY-SUBRC = 0.
      READ TABLE LT_RESOURCE_FLG
      INTO LS_RESOURCE
      WITH KEY
      AUDITTRAIL = ORIG_AUDITTRAIL
      CATEGORY   = ORIG_CATEGORY
      EMPLOYEE   = ORIG_EMPLOYEE
      ENTITY     = ORIG_ENTITY
      RESOURCE_ACCT  = 'RP900006'
      RPTCURRENCY    = ORIG_CURRENCY
      TIME           = 'XXXX.INP'
      BINARY SEARCH.
      IF SY-SUBRC = 0.
        FACTOR1 = LS_RESOURCE-SIGNEDDATA.
      ELSE.
        FACTOR1 = 0.
      ENDIF.

      CASE FACTOR1.

        WHEN 10 .
          ACCOUNT_DRIVER = 'RP000100'.
        WHEN 11 .
          ACCOUNT_DRIVER = 'RP000101'.
        WHEN 12 .
          ACCOUNT_DRIVER = 'RP000102'.
        WHEN 13 .
          ACCOUNT_DRIVER = 'RP000103'.
        WHEN 14 .
          ACCOUNT_DRIVER = 'RP000104'.
        WHEN 15 .
          ACCOUNT_DRIVER = 'RP000105'.
        WHEN 16 .
          ACCOUNT_DRIVER = 'RP000106'.
        WHEN 17 .
          ACCOUNT_DRIVER = 'RP000107'.
        WHEN OTHERS.
          ACCOUNT_DRIVER = 'DONOTHING'.
        ENDCASE.

        CLEAR LS_RESOURCE.
        READ TABLE LT_RESOURCE_FLG
        INTO LS_RESOURCE
        WITH KEY
        AUDITTRAIL      = ORIG_AUDITTRAIL
        CATEGORY        = ORIG_CATEGORY
        EMPLOYEE        = 'NO_EMPLOYEE'
        ENTITY          = ENTITY_DRIVER
        RESOURCE_ACCT   =  ACCOUNT_DRIVER
        RPTCURRENCY     = ORIG_CURRENCY
        TIME            = YEAR_INP
        BINARY SEARCH.
        IF SY-SUBRC = 0.
          <LS_SIGNEDDATA>     = LS_RESOURCE-SIGNEDDATA.
          <LS_RESOURCE_ACCT>  = 'RP010008' .
          <LS_TIME>           = YEAR_INP.
          COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

          ENDIF.
    ENDIF.

    ENDIF.

  WHEN OTHERS.

  ENDCASE.

    ENDLOOP. "MAIN LOOP END


    CLEAR CT_DATA.
    APPEND LINES OF <LT_FINAL> TO CT_DATA.

    IF SUB_ID = 'NONE' AND GATE5 = 1. "CHANGE3
      LOOP AT LT_RESOURCE_FLG2 INTO LS_RESOURCE.
        CLEAR LS_CTDATA.
            LS_CTDATA-AUDITTRAIL   = LS_RESOURCE-AUDITTRAIL.
            LS_CTDATA-CATEGORY    = LS_RESOURCE-CATEGORY.
            LS_CTDATA-EMPLOYEE    = LS_RESOURCE-EMPLOYEE.
            LS_CTDATA-ENTITY      = LS_RESOURCE-ENTITY.
            LS_CTDATA-RESOURCE_ACCT = LS_RESOURCE-RESOURCE_ACCT.
            LS_CTDATA-RPTCURRENCY   = LS_RESOURCE-RPTCURRENCY.
            LS_CTDATA-TIME          = LS_RESOURCE-TIME.
            LS_CTDATA-SIGNEDDATA    = LS_RESOURCE-SIGNEDDATA.
            LS_CTDATA-MEASURES    = ''.

        APPEND  LS_CTDATA TO CT_DATA.

      ENDLOOP.


    ENDIF.

  ENDMETHOD.


  METHOD TRANSFER_CP_IS. "Transfer Capex to IS
*&---------------------------------------------------------------------*
*&  Method           TRANSFER_CP_IS
*&---------------------------------------------------------------------*
****************************************************************************
* Program type....:  Customer SO Custom Method
* Author...........:   ()
* Date.............: February 2017
* ----------------------------------------------------------------------
* Description......: This method handles data push from CAPEX model
*                    to INC_STATEMENT model of Customer SO_PLANNING environment
*                    doing certain conversions on the way
*
****************************************************************************
* Change Log
****************************************************************************
* Changed on:           By:
* Description:
****************************************************************************

INCLUDE ZINC_PLAN_BADI_COMMON_VARS. "Common Variables

*** SET TOOLS INSTANCE *****
    CREATE OBJECT LO_TOOLS
      EXPORTING
        I_ENV_ID = I_APPSET_ID
        I_MOD_ID = I_APPL_ID
        I_T_PARAM = IT_PARAM
        I_T_CV = IT_CV.
***************************************************************
************** ASSIGN WAs *************************************
    CREATE DATA LT_FINAL LIKE CT_DATA.
    ASSIGN LT_FINAL->* TO <LT_FINAL>.
    CREATE DATA LS_RESULT_REC LIKE LINE OF CT_DATA.
    ASSIGN LS_RESULT_REC->* TO <LS_RESULT_REC>.
    CREATE DATA LS_REC LIKE LINE OF CT_DATA.
    ASSIGN LS_REC->* TO <LS_REC>.
***************************************************************************

*****************************************************************************
*****************************************************************************
****** GET TIMESCOPE AND VARIABLES
    LO_TOOLS->TIME(
      EXPORTING
        CATEGORY    = WORKING_CATEGORY
      IMPORTING
       TIMESCOPE   = LT_TIMESCOPE ).

***** END GET TIMESCOPE AND VARIABLES

********* GET SELF *************************************
"Expand scope

    LO_TOOLS->WRITE_MODEL_DATA(
      EXPORTING
        TARGET_MODEL = I_APPL_ID "self structure
        WRITE       = 'N'
     IMPORTING
       LR_DATA     = LRR_DATA ).

    ASSIGN LRR_DATA->* TO <CT_DATA>.


        LOOP AT LT_TIMESCOPE INTO LS_TIME.

          LS_SEL-DIMENSION = 'TIME'.
          LS_SEL-ATTRIBUTE = 'ID'.
          LS_SEL-SIGN = 'I'.
          LS_SEL-OPTION = 'EQ'.
          LS_SEL-LOW = LS_TIME-ID.
          APPEND LS_SEL TO LT_SEL.

        ENDLOOP.

LO_TOOLS->READ_MODEL_DATA(
  EXPORTING
    IT_SEL      = LT_SEL
    IT_CV = IT_CV
  IMPORTING
   OUTPUT_DATA = <CT_DATA> ).

  CLEAR CT_DATA.
  MOVE-CORRESPONDING <CT_DATA> TO CT_DATA.

*************** GET INC_STATEMENT TYPE MODEL **********

    LO_TOOLS->WRITE_MODEL_DATA(
      EXPORTING
       TARGET_MODEL  = 'INC_STATEMENT'
        WRITE       = 'N'
      IMPORTING
       LR_DATA     = LR_DATA ).
******************************************************

    ASSIGN LR_DATA->* TO <TARGET_DATA>.
    CREATE DATA LR_TARGET_LINE LIKE LINE OF <TARGET_DATA>.
    ASSIGN LR_TARGET_LINE->* TO <TARGET_LINE>.

    ASSIGN COMPONENT 'ACCOUNT' OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_ACCOUNT>.
    ASSIGN COMPONENT 'AUDITTRAIL' OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_AUDITTRAIL>.
    ASSIGN COMPONENT 'ANALYSIS' OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_ANALYSIS>.
    ASSIGN COMPONENT 'ENTITY' OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_ENTITY>.
    ASSIGN COMPONENT 'CATEGORY' OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_CATEGORY>.
    ASSIGN COMPONENT 'TIME' OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_TIME>.
    ASSIGN COMPONENT 'PARTNER' OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_PARTNER>.
    ASSIGN COMPONENT 'RPTCURRENCY' OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_RPTCURRENCY>.
    ASSIGN COMPONENT 'SIGNEDDATA' OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_SIGNEDDATA>.

*********************************************************************************************

******************* SOURCE STRUCTURE ********************************************************
    LOOP AT CT_DATA ASSIGNING <LS_RESULT_REC>.

      ASSIGN COMPONENT 'ASSET_CLASS' OF STRUCTURE <LS_RESULT_REC> TO <LS_ASSET_CLASS>.
      ASSIGN COMPONENT 'FLOW' OF STRUCTURE <LS_RESULT_REC> TO <LS_FLOW>.
      ASSIGN COMPONENT 'AUDITTRAIL' OF STRUCTURE <LS_RESULT_REC> TO <LS_AUDITTRAIL>.
      ASSIGN COMPONENT 'ENTITY' OF STRUCTURE <LS_RESULT_REC> TO <LS_ENTITY>.
      ASSIGN COMPONENT 'CATEGORY' OF STRUCTURE <LS_RESULT_REC> TO <LS_CATEGORY>.
      ASSIGN COMPONENT 'TIME' OF STRUCTURE <LS_RESULT_REC> TO <LS_TIME>.
      ASSIGN COMPONENT 'RPTCURRENCY' OF STRUCTURE <LS_RESULT_REC> TO <LS_RPTCURRENCY>.
      ASSIGN COMPONENT 'SIGNEDDATA' OF STRUCTURE <LS_RESULT_REC> TO <LS_SIGNEDDATA>.

       CHECK <LS_AUDITTRAIL> = 'INPUT'.

      <LS_TARGET_TIME> = <LS_TIME>.
      <LS_TARGET_ACCOUNT>   = <LS_ASSET_CLASS> .
      <LS_TARGET_AUDITTRAIL> =  'CAPEX_INPUT'.
      <LS_TARGET_ANALYSIS>   = <LS_FLOW>.
      <LS_TARGET_CATEGORY>  = <LS_CATEGORY>.
      <LS_TARGET_ENTITY>  = <LS_ENTITY>.
      <LS_TARGET_RPTCURRENCY> = <LS_RPTCURRENCY>.
      <LS_TARGET_PARTNER>    = 'I_NONE'.
      <LS_TARGET_SIGNEDDATA> = <LS_SIGNEDDATA>.

      COLLECT <TARGET_LINE> INTO <TARGET_DATA>.
    ENDLOOP.

***** READY TO WRITE BACK *******
    LO_TOOLS->WRITE_MODEL_DATA(
      EXPORTING
       TARGET_MODEL     = 'INC_STATEMENT'
       INPUT_DATA  = <TARGET_DATA>
        WRITE  = 'Y'
      IMPORTING
       ET_MESSAGE  = ET_MESSAGE
     "  ET_ERROR_RECORDS = ET_ERROR_RECORDS
       ).

*********************************
CLEAR CT_DATA.

  ENDMETHOD.


  METHOD TRANSFER_IS_BS.
*&---------------------------------------------------------------------*
*&  Method           TRANSFER_IS_BS
*&---------------------------------------------------------------------*
****************************************************************************
* Program type....:  Customer SO Custom Method
* Author...........:   ()
* Date.............: February 2017
* ----------------------------------------------------------------------
* Description......: This method handles data push from INC_STATEMENT model
*                    to CASHFLOW model of Customer SO_PLANNING environment
*                    doing certain conversions on the way
*
****************************************************************************
* Change Log
****************************************************************************
* Changed on:                 By:
* Description:
****************************************************************************
INCLUDE ZINC_PLAN_BADI_COMMON_VARS. "Common Variables

*** SET TOOLS INSTANCE *****
    CREATE OBJECT LO_TOOLS
      EXPORTING
        I_ENV_ID = I_APPSET_ID
        I_MOD_ID = I_APPL_ID
        I_T_PARAM = IT_PARAM
        I_T_CV = IT_CV.
***************************************************************
************** ASSIGN WAs *************************************
    CREATE DATA LT_FINAL LIKE CT_DATA.
    ASSIGN LT_FINAL->* TO <LT_FINAL>.
    CREATE DATA LS_RESULT_REC LIKE LINE OF CT_DATA.
    ASSIGN LS_RESULT_REC->* TO <LS_RESULT_REC>.
    CREATE DATA LS_REC LIKE LINE OF CT_DATA.
    ASSIGN LS_REC->* TO <LS_REC>.
***************************************************************************

*****************************************************************************
*****************************************************************************
****** GET TIMESCOPE AND VARIABLES
    LO_TOOLS->TIME(
      EXPORTING
        CATEGORY    = WORKING_CATEGORY
      IMPORTING
       TIMESCOPE   = LT_TIMESCOPE ).

***** END GET TIMESCOPE AND VARIABLES


*************CHANGE THE TIMSCOPE when SUB_ID = TIMEX
CLEAR: LS_PARAM.
READ TABLE IT_PARAM WITH KEY HASHKEY = 'SUB_ID' INTO LS_PARAM.

IF LS_PARAM-HASHVALUE = 'TIMEX'.
  READ TABLE IT_CV INTO CV_TIME WITH TABLE KEY DIM_UPPER_CASE = 'TIME'.
  CLEAR: LS_TIME, LT_TIMESCOPE.
    LOOP AT CV_TIME-MEMBER INTO WA_MEMBER.
      LS_TIME-ID = WA_MEMBER.
      APPEND LS_TIME TO LT_TIMESCOPE.
    ENDLOOP.
ENDIF.
************* END CHANGE THE TIMSCOPE when SUB_ID = TIMEX


********* GET SELF *************************************
"Expand scope

    LO_TOOLS->WRITE_MODEL_DATA(
      EXPORTING
        TARGET_MODEL = I_APPL_ID "self structure
        WRITE       = 'N'
     IMPORTING
       LR_DATA     = LRR_DATA ).

    ASSIGN LRR_DATA->* TO <CT_DATA>.


        LOOP AT LT_TIMESCOPE INTO LS_TIME.

          LS_SEL-DIMENSION = 'TIME'.
          LS_SEL-ATTRIBUTE = 'ID'.
          LS_SEL-SIGN = 'I'.
          LS_SEL-OPTION = 'EQ'.
          LS_SEL-LOW = LS_TIME-ID.
          APPEND LS_SEL TO LT_SEL.

        ENDLOOP.

LO_TOOLS->READ_MODEL_DATA(
  EXPORTING
    IT_SEL      = LT_SEL
    IT_CV = IT_CV
  IMPORTING
   OUTPUT_DATA = <CT_DATA> ).

  CLEAR CT_DATA.
  MOVE-CORRESPONDING <CT_DATA> TO CT_DATA.

*****************************************************************************

******* GET ALL ANALYSIS ********************************
    ANALYSIS = LO_TOOLS->GET_PARENT_MD(
    EXPORTING
    PARENT_MBR = 'ALL_ANALYSIS'
    I_DIMENSION_ID = 'ANALYSIS').
**************************************************************

***** GET ENTITY FROM IT_CV **********************************

    READ TABLE IT_CV INTO CV_ENTITY WITH TABLE KEY DIM_UPPER_CASE = 'ENTITY'.

    CLEAR: LT_SEL, LS_SEL.

    LOOP AT CV_ENTITY-MEMBER INTO WA_MEMBER.

      LS_SEL-DIMENSION = 'ENTITY'.
      LS_SEL-ATTRIBUTE = 'ID'.
      LS_SEL-SIGN = 'I'.
      LS_SEL-OPTION = 'EQ'.
      LS_SEL-LOW = WA_MEMBER.
      APPEND LS_SEL TO LT_SEL.
    ENDLOOP.

******** GET MASTER DATA FOR ENTITY ******************************
    LO_TOOLS->READ_MASTER_DATA(
      EXPORTING
        I_DIMENSION_ID   = LS_SEL-DIMENSION
      IMPORTING
        OUTPUT_R_DATA = MASTER_DATA
      CHANGING
        LT_SEL        = LT_SEL ).

    ASSIGN MASTER_DATA->* TO <DIMENSION_DATA>.
    MOVE-CORRESPONDING <DIMENSION_DATA> TO LT_ENTITY.
*******************************************************************

*************** GET INC_STATEMENT TYPE MODEL **********

    LO_TOOLS->WRITE_MODEL_DATA(
      EXPORTING
       TARGET_MODEL  = 'CASHFLOW'
        WRITE       = 'N'
      IMPORTING
       LR_DATA     = LR_DATA ).


    ASSIGN LR_DATA->* TO <TARGET_DATA>.
    CREATE DATA LR_TARGET_LINE LIKE LINE OF <TARGET_DATA>.
    ASSIGN LR_TARGET_LINE->* TO <TARGET_LINE>.

    ASSIGN COMPONENT 'ACCOUNT' OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_ACCOUNT>.
    ASSIGN COMPONENT 'AUDITTRAIL' OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_AUDITTRAIL>.
    ASSIGN COMPONENT 'FLOW' OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_FLOW>.
    ASSIGN COMPONENT 'PROFIT_CENTRE' OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_PROFIT_CENTRE>.
    ASSIGN COMPONENT 'CATEGORY' OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_CATEGORY>.
    ASSIGN COMPONENT 'TIME' OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_TIME>.
    ASSIGN COMPONENT 'PARTNER' OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_PARTNER>.
    ASSIGN COMPONENT 'RPTCURRENCY' OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_RPTCURRENCY>.
    ASSIGN COMPONENT 'SIGNEDDATA' OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_SIGNEDDATA>.

*********************************************************************************************

    COUNTER = 0.

    LOOP AT CT_DATA ASSIGNING <LS_RESULT_REC>.

      ASSIGN COMPONENT 'ACCOUNT' OF STRUCTURE <LS_RESULT_REC> TO <LS_ACCOUNT>.
      ASSIGN COMPONENT 'ANALYSIS' OF STRUCTURE <LS_RESULT_REC> TO <LS_ANALYSIS>.
      ASSIGN COMPONENT 'AUDITTRAIL' OF STRUCTURE <LS_RESULT_REC> TO <LS_AUDITTRAIL>.
      ASSIGN COMPONENT 'ENTITY' OF STRUCTURE <LS_RESULT_REC> TO <LS_ENTITY>.
      ASSIGN COMPONENT 'CATEGORY' OF STRUCTURE <LS_RESULT_REC> TO <LS_CATEGORY>.
      ASSIGN COMPONENT 'TIME' OF STRUCTURE <LS_RESULT_REC> TO <LS_TIME>.
      ASSIGN COMPONENT 'PARTNER' OF STRUCTURE <LS_RESULT_REC> TO <LS_PARTNER>.
      ASSIGN COMPONENT 'RPTCURRENCY' OF STRUCTURE <LS_RESULT_REC> TO <LS_RPTCURRENCY>.
      ASSIGN COMPONENT 'SIGNEDDATA' OF STRUCTURE <LS_RESULT_REC> TO <LS_SIGNEDDATA>.

      CLEAR: LS_ENTITY.

      READ TABLE LT_TIMESCOPE WITH KEY ID = <LS_TIME> TRANSPORTING NO FIELDS.
      IF SY-SUBRC NE 0.
        DELETE CT_DATA.
        CONTINUE.
      ENDIF.

*      IF <LS_RPTCURRENCY> <> 'LC'.
*        DELETE CT_DATA.
*        CONTINUE.
*      ENDIF.

      IF <LS_AUDITTRAIL> = 'INPUT'.
        <LS_AUDITTRAIL> = 'IS_INPUT'.
      ENDIF.

      READ TABLE ANALYSIS WITH KEY TABLE_LINE = <LS_ANALYSIS> TRANSPORTING NO FIELDS.
      IF SY-SUBRC = 0.
        <LS_ANALYSIS> = 'PL99'.
      ENDIF.

      READ TABLE LT_ENTITY WITH KEY ID = <LS_ENTITY> INTO LS_ENTITY.

      IF LS_ENTITY-PROFIT_CENTRE IS INITIAL.
        CLEAR: L_LOG.

        IF COUNTER = 0.
          MESSAGE e006(ZCL_BPC_PLANNING_MSG) INTO L_LOG WITH 'PROFIT_CENTRE' 'ENTITY'.
          CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).
          L_LOG = LS_ENTITY-ID.
          CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).

          CLEAR ET_MESSAGE.

          LS_MESSAGE-MSGID = 'BPC' .
          LS_MESSAGE-MSGTY = 'E'.
          LS_MESSAGE-MSGNO = '010' .
          MESSAGE e007(ZCL_BPC_PLANNING_MSG) INTO LS_MESSAGE-MESSAGE WITH 'Income Statement' 'Cashflow'.
          APPEND LS_MESSAGE TO ET_MESSAGE.

          COUNTER = 1.
        ELSE.

          L_LOG = LS_ENTITY-ID.
          CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).
        ENDIF.

      ELSE.

        <LS_ENTITY> = LS_ENTITY-PROFIT_CENTRE.

      ENDIF.

      IF ET_MESSAGE IS NOT INITIAL.
        RAISE EXCEPTION TYPE CX_UJ_CUSTOM_LOGIC.
        EXIT.
      ENDIF.

      <LS_TARGET_TIME>      = <LS_TIME>.
      <LS_TARGET_ACCOUNT>   = <LS_ACCOUNT> .
      <LS_TARGET_AUDITTRAIL> =  <LS_AUDITTRAIL>.
      <LS_TARGET_FLOW>   = <LS_ANALYSIS>.
      <LS_TARGET_CATEGORY>  = <LS_CATEGORY>.
      <LS_TARGET_PROFIT_CENTRE>  = <LS_ENTITY>.
      <LS_TARGET_RPTCURRENCY> = <LS_RPTCURRENCY>.
      <LS_TARGET_PARTNER>    = <LS_PARTNER>.
      <LS_TARGET_SIGNEDDATA> = <LS_SIGNEDDATA>.

      COLLECT <TARGET_LINE> INTO <TARGET_DATA>.
    ENDLOOP.



***** READY TO WRITE BACK *******
    LO_TOOLS->WRITE_MODEL_DATA(
      EXPORTING
       TARGET_MODEL     = 'CASHFLOW'
       INPUT_DATA  = <TARGET_DATA>
        WRITE  = 'Y'
      IMPORTING
       ET_MESSAGE  = ET_MESSAGE
       "ET_ERROR_RECORDS = ET_ERROR_RECORDS
       ).

    CLEAR CT_DATA.


  ENDMETHOD.


  METHOD TRANSFER_RS_IS. "Transfer Capex to IS
*&---------------------------------------------------------------------*
*&  Method           TRANSFER_RS_IS
*&---------------------------------------------------------------------*
****************************************************************************
* Program type....:  Customer SO Custom Method
* Author...........:   ()
* Date.............: February 2017
* ----------------------------------------------------------------------
* Description......: This method handles data push from RESOURCE model
*                    to INC_STATEMENT model of Customer SO_PLANNING environment
*                    doing certain conversions on the way. Only Children of
*                    TOTAL_STATACCT and RP000408
*
****************************************************************************
* Change Log: 1. 27072017 - Change ITS CREDIT transfer audittrail to RESOURCE_ITSCREDIT
*                this allows ITS Credit to be tracked in IS properly.
*             2. 27072017 - Change in ITS transfer sign flip on ITSS DEMAND Accounts.
*                Currently it's only one AC710355 and it's already created negative,
*                additional flip of sign is not required.
*             3. 28092017 - ITS_CREDIT to be assigned as audittrail under ITS_C clause.
*
****************************************************************************
* Changed on: 27 JUL 2017             By:
* Description: Addition of Intercompany look up
****************************************************************************

INCLUDE ZINC_PLAN_BADI_COMMON_VARS. "Common Variables

*** SET TOOLS INSTANCE *****
    CREATE OBJECT LO_TOOLS
      EXPORTING
        I_ENV_ID = I_APPSET_ID
        I_MOD_ID = I_APPL_ID
        I_T_PARAM = IT_PARAM
        I_T_CV = IT_CV.
***************************************************************
************** ASSIGN WAs *************************************
    CREATE DATA LT_FINAL LIKE CT_DATA.
    ASSIGN LT_FINAL->* TO <LT_FINAL>.
    CREATE DATA LS_RESULT_REC LIKE LINE OF CT_DATA.
    ASSIGN LS_RESULT_REC->* TO <LS_RESULT_REC>.
    CREATE DATA LS_REC LIKE LINE OF CT_DATA.
    ASSIGN LS_REC->* TO <LS_REC>.
***************************************************************************

*****************************************************************************
****** GET TIMESCOPE AND VARIABLES
    LO_TOOLS->TIME(
      EXPORTING
        CATEGORY    = WORKING_CATEGORY
      IMPORTING
       TIMESCOPE   = LT_TIMESCOPE ).

***** END GET TIMESCOPE AND VARIABLES

***** GET EMPLOYEE REFERENCE  *************************************
    CLEAR: LS_SEL , LT_SEL.
    LS_SEL-DIMENSION = 'EMPLOYEE'.

    LO_TOOLS->READ_MASTER_DATA(
      EXPORTING
        I_DIMENSION_ID   = LS_SEL-DIMENSION
      IMPORTING
        OUTPUT_R_DATA = MASTER_DATA
      CHANGING
        LT_SEL        = LT_SEL ).

    ASSIGN MASTER_DATA->* TO <DIMENSION_DATA>.
    MOVE-CORRESPONDING <DIMENSION_DATA> TO LT_EMPLOYEE.

********************************************************************
***** GET ENTITY REFERENCE  *************************************
    CLEAR: LS_SEL , LT_SEL.
    LS_SEL-DIMENSION = 'ENTITY'.

    LO_TOOLS->READ_MASTER_DATA(
      EXPORTING
        I_DIMENSION_ID   = LS_SEL-DIMENSION
      IMPORTING
        OUTPUT_R_DATA = MASTER_DATA
      CHANGING
        LT_SEL        = LT_SEL ).

    ASSIGN MASTER_DATA->* TO <DIMENSION_DATA>.
    MOVE-CORRESPONDING <DIMENSION_DATA> TO LT_ENTITY.
************************************************************************

"GET REF ENTITY COMING FROM THE IT_CV
    "READ ENTITY THAT ARE IN THE IT_CV TABLE
    CLEAR: LS_SEL , LT_SEL.

    READ TABLE IT_CV INTO CV_ENTITY WITH TABLE KEY DIM_UPPER_CASE = 'ENTITY'.
    LOOP AT CV_ENTITY-MEMBER INTO WA_MEMBER.
      APPEND WA_MEMBER TO LT_CV_ENTITY.
      LS_SEL-DIMENSION = 'ENTITY'.
      LS_SEL-ATTRIBUTE = 'ID'.
      LS_SEL-SIGN = 'I'.
      LS_SEL-OPTION = 'EQ'.
      LS_SEL-LOW = WA_MEMBER.
      APPEND LS_SEL TO LT_SEL.
      ENDLOOP.

    LO_TOOLS->READ_MASTER_DATA(
      EXPORTING
        I_DIMENSION_ID   = LS_SEL-DIMENSION
      IMPORTING
        OUTPUT_R_DATA = MASTER_DATA
      CHANGING
        LT_SEL        = LT_SEL ).

    ASSIGN MASTER_DATA->* TO <DIMENSION_DATA>.
    MOVE-CORRESPONDING <DIMENSION_DATA> TO LT_ENTITY_CV.

    LOOP AT LT_ENTITY_CV INTO LS_ENTITY.
      APPEND LS_ENTITY-PROFIT_CENTRE TO LT_PROFITCTR.
        ENDLOOP.

     SORT LT_PROFITCTR.
     DELETE ADJACENT DUPLICATES FROM LT_PROFITCTR.


***** GET RESOURCE_ACCT REFERENCE  *************************************
    CLEAR: LS_SEL , LT_SEL.
    LS_SEL-DIMENSION = 'RESOURCE_ACCT'.

    LO_TOOLS->READ_MASTER_DATA(
      EXPORTING
        I_DIMENSION_ID   = LS_SEL-DIMENSION
      IMPORTING
        OUTPUT_R_DATA = MASTER_DATA
      CHANGING
        LT_SEL        = LT_SEL ).

    ASSIGN MASTER_DATA->* TO <DIMENSION_DATA>.
    MOVE-CORRESPONDING <DIMENSION_DATA> TO LT_RESOURCE_ACCT.
*******************************************************************
***** GET CHILDREN OF 'TOTAL_STATACCT'*****************************
    CLEAR: LS_SEL , LT_SEL.
    LS_SEL-DIMENSION = 'RESOURCE_ACCT'.

    LT_STATACCT = LO_TOOLS->GET_PARENT_MD( EXPORTING
      PARENT_MBR = 'TOTAL_STATACCT'
      I_DIMENSION_ID = LS_SEL-DIMENSION ).

    SORT LT_STATACCT.
*******************************************************************
***** GET CHILDREN OF 'IS_GLACCT'****'*****************************
    CLEAR: LS_SEL , LT_SEL.
    LS_SEL-DIMENSION = 'RESOURCE_ACCT'.

    LT_IS_GLACCT = LO_TOOLS->GET_PARENT_MD( EXPORTING
      PARENT_MBR = 'IS_GLACCT'
      I_DIMENSION_ID = LS_SEL-DIMENSION ).

    SORT LT_IS_GLACCT.
*******************************************************************
***** GET CHILDREN OF 'BS_OTHACCT'*****'***************************
    CLEAR: LS_SEL , LT_SEL.
    LS_SEL-DIMENSION = 'RESOURCE_ACCT'.

    "LT_BS_COSTACCT = LO_TOOLS->GET_PARENT_MD( EXPORTING
    LT_BS_OTHACCT = LO_TOOLS->GET_PARENT_MD( EXPORTING
      PARENT_MBR = 'BS_OTHACCT'  "'BS_COSTACCT'
      I_DIMENSION_ID = LS_SEL-DIMENSION ).

    SORT LT_BS_OTHACCT .  "LT_BS_COSTACCT.
*******************************************************************
***** GET CHILDREN OF 'BS_ICOTHACCT'*****'***************************
    CLEAR: LS_SEL , LT_SEL.
    LS_SEL-DIMENSION = 'RESOURCE_ACCT'.

    LT_BS_ICOTHACCT = LO_TOOLS->GET_PARENT_MD( EXPORTING
      PARENT_MBR = 'BS_ICOTHACCT'  "'BS_COSTACCT'
      I_DIMENSION_ID = LS_SEL-DIMENSION ).

    SORT LT_BS_ICOTHACCT.
*******************************************************************

***** GET CHILDREN OF 'IS_ITSS_DEMAND'*****************************
    CLEAR: LS_SEL , LT_SEL.
    LS_SEL-DIMENSION = 'RESOURCE_ACCT'.

    LT_IS_ITSS_DEMAND = LO_TOOLS->GET_PARENT_MD( EXPORTING
      PARENT_MBR = 'IS_ITSS_DEMAND'
      I_DIMENSION_ID = LS_SEL-DIMENSION ).

    SORT LT_IS_ITSS_DEMAND.
*******************************************************************
***** GET CHILDREN OF 'BS_ITSS_DEMAND'*****************************
    CLEAR: LS_SEL , LT_SEL.
    LS_SEL-DIMENSION = 'RESOURCE_ACCT'.

    LT_BS_ITSS_DEMAND = LO_TOOLS->GET_PARENT_MD( EXPORTING
      PARENT_MBR = 'BS_ITSS_DEMAND'
      I_DIMENSION_ID = LS_SEL-DIMENSION ).

    SORT LT_BS_ITSS_DEMAND.
*******************************************************************
***** GET CHILDREN OF 'IS_ITSS_SUPPLY'*****************************
    CLEAR: LS_SEL , LT_SEL.
    LS_SEL-DIMENSION = 'RESOURCE_ACCT'.

    LT_IS_ITSS_SUPPLY = LO_TOOLS->GET_PARENT_MD( EXPORTING
      PARENT_MBR = 'IS_ITSS_SUPPLY'
      I_DIMENSION_ID = LS_SEL-DIMENSION ).

    SORT LT_IS_ITSS_SUPPLY.
*******************************************************************
***** GET CHILDREN OF 'BS_ITSS_DEMAND'*****************************
    CLEAR: LS_SEL , LT_SEL.
    LS_SEL-DIMENSION = 'RESOURCE_ACCT'.

    LT_BS_ITSS_SUPPLY = LO_TOOLS->GET_PARENT_MD( EXPORTING
      PARENT_MBR = 'BS_ITSS_SUPPLY'
      I_DIMENSION_ID = LS_SEL-DIMENSION ).

    SORT LT_BS_ITSS_SUPPLY.
*******************************************************************

***** GET CHILDREN OF 'TOTAL_ITSSROLES'****************************
    CLEAR: LS_SEL , LT_SEL.
    LS_SEL-DIMENSION = 'EMPLOYEE'.

    LT_ITSSROLES = LO_TOOLS->GET_PARENT_MD( EXPORTING
      PARENT_MBR = 'TOTAL_ITSSROLES'
      I_DIMENSION_ID = LS_SEL-DIMENSION ).

    SORT LT_ITSSROLES.
*******************************************************************
********* GET SELF ************************************************

    LO_TOOLS->WRITE_MODEL_DATA(
      EXPORTING
        TARGET_MODEL = I_APPL_ID "RESOURCE MODEL
        WRITE       = 'N'
     IMPORTING
       LR_DATA     = LRR_DATA ).

    ASSIGN LRR_DATA->* TO <CT_DATA>.

CLEAR: LS_SEL , LT_SEL.
"Expand scope
    LOOP AT LT_TIMESCOPE INTO LS_TIME.

      LS_SEL-DIMENSION = 'TIME'.
      LS_SEL-ATTRIBUTE = 'ID'.
      LS_SEL-SIGN = 'I'.
      LS_SEL-OPTION = 'EQ'.
      LS_SEL-LOW = LS_TIME-ID.
      APPEND LS_SEL TO LT_SEL.

    ENDLOOP.

"READ MODEL TO BUILD UP CT_DATA
LO_TOOLS->READ_MODEL_DATA(
  EXPORTING
    IT_SEL      = LT_SEL
    IT_CV       = IT_CV
  IMPORTING
   OUTPUT_DATA  = <CT_DATA> ).

  CLEAR CT_DATA.
  MOVE-CORRESPONDING <CT_DATA> TO CT_DATA.

*************** GET INC_STATEMENT TYPE MODEL **************

    LO_TOOLS->WRITE_MODEL_DATA(
      EXPORTING
       TARGET_MODEL  = 'INC_STATEMENT'
        WRITE       = 'N'
      IMPORTING
       LR_DATA     = LR_DATA ).

***********************************************************

    ASSIGN LR_DATA->* TO <TARGET_DATA>.
    CREATE DATA LR_TARGET_LINE LIKE LINE OF <TARGET_DATA>.
    ASSIGN LR_TARGET_LINE->* TO <TARGET_LINE>.

    ASSIGN COMPONENT 'ACCOUNT'      OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_ACCOUNT>.
    ASSIGN COMPONENT 'AUDITTRAIL'   OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_AUDITTRAIL>.
    ASSIGN COMPONENT 'ANALYSIS'     OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_ANALYSIS>.
    ASSIGN COMPONENT 'ENTITY'       OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_ENTITY>.
    ASSIGN COMPONENT 'CATEGORY'     OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_CATEGORY>.
    ASSIGN COMPONENT 'TIME'         OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_TIME>.
    ASSIGN COMPONENT 'PARTNER'      OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_PARTNER>.
    ASSIGN COMPONENT 'RPTCURRENCY'  OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_RPTCURRENCY>.
    ASSIGN COMPONENT 'SIGNEDDATA'   OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_SIGNEDDATA>.

*********************************************************************************************
****EXPAND SCOPE TO FETCH ALL DATA THAT CAME FROM RESOURCE
****JUST IN CASE THE ENTITY GOES MISSING IN RESOURCE DUE TO ZERO ELIMINATION
****ALTHOUGH THIS MIGHT MAKE THE MEMORY CONSUMPTION TOO BIG
****TRIM THIS DOWN BY LIMITNG THE PROFIT CENTRE PARENTS.
****CLEAR: LS_SEL , LT_SEL.
****    LOOP AT LT_TIMESCOPE INTO LS_TIME.
****      LS_SEL-DIMENSION = 'TIME'.
****      LS_SEL-ATTRIBUTE = 'ID'.
****      LS_SEL-SIGN = 'I'.
****      LS_SEL-OPTION = 'EQ'.
****      LS_SEL-LOW = LS_TIME-ID.
****      APPEND LS_SEL TO LT_SEL.
****    ENDLOOP.
****
****    LS_SEL-DIMENSION = 'RPTCURRENCY'.
****    LS_SEL-ATTRIBUTE = 'ID'.
****    LS_SEL-SIGN = 'I'.
****    LS_SEL-OPTION = 'EQ'.
****    LS_SEL-LOW = 'LC'.
****    APPEND LS_SEL TO LT_SEL.
****
****    LS_SEL-DIMENSION = 'CATEGORY'.
****    LS_SEL-ATTRIBUTE = 'ID'.
****    LS_SEL-SIGN = 'I'.
****    LS_SEL-OPTION = 'EQ'.
****    LS_SEL-LOW = WORKING_CATEGORY.
****    APPEND LS_SEL TO LT_SEL.
****
****    LS_SEL-DIMENSION = 'AUDITTRAIL'.
****    LS_SEL-ATTRIBUTE = 'ID'.
****    LS_SEL-SIGN = 'I'.
****    LS_SEL-OPTION = 'EQ'.
****    LS_SEL-LOW = 'RESOURCE_INPUT'.
****    APPEND LS_SEL TO LT_SEL.
****
****    PUT HERE READ MODEL TARGET
****READ MODEL TO BUILD UP CT_DATA
****
****LO_TOOLS->READ_MODEL_DATA(
****  EXPORTING
****    READ_MODEL  = 'INC_STATEMENT'
****    IT_SEL      = LT_SEL
****  IMPORTING
****   OUTPUT_DATA  = <TARGET_DATA> ).
****
****  CLEAR:  LT_INC_STATEMENT , LT_INC_STATEMENT0 .
****  MOVE-CORRESPONDING <TARGET_DATA> TO LT_INC_STATEMENT.
****  CLEAR <TARGET_DATA>.
****
*******  LOOP AT LT_INC_STATEMENT ASSIGNING <LS_INC_STATEMENT>.
*******    " CHECK IF IS PART OF CV_ENTITY
*******    READ TABLE LT_CV_ENTITY TRANSPORTING NO FIELDS
*******    WITH KEY TABLE_LINE = <LS_INC_STATEMENT>-ENTITY.
*******    IF SY-SUBRC = 0.
*******      <LS_INC_STATEMENT>-SIGNEDDATA = 0 .
*******      APPEND <LS_INC_STATEMENT> TO LT_INC_STATEMENT0 .
*******      ELSE.
*******        "CHECK IF THE PROFIT CENTRE IS PART OF THE IT_CV 'S
*******        READ TABLE LT_ENTITY INTO LS_ENTITY
*******        WITH KEY ID = <LS_INC_STATEMENT>-ENTITY.
*******        IF SY-SUBRC = 0 .
*******          READ TABLE LT_PROFITCTR INTO LS_PROFITCTR
*******          WITH KEY PROFIT_CENTRE = LS_ENTITY-PROFIT_CENTRE.
*******          IF SY-SUBRC = 0.
*******          <LS_INC_STATEMENT>-SIGNEDDATA = 0 .
*******          APPEND <LS_INC_STATEMENT> TO LT_INC_STATEMENT0 .
*******            ENDIF.
*******          ENDIF.
*******        ENDIF.
*******    ENDLOOP.
*******
*******  "MOVE-CORRESPONDING LT_INC_STATEMENT0 TO <TARGET_DATA>.

****************************************************************
*******
*******    ASSIGN LR_DATA->* TO <TARGET_DATA>.
*******    CREATE DATA LR_TARGET_LINE LIKE LINE OF <TARGET_DATA>.
*******    ASSIGN LR_TARGET_LINE->* TO <TARGET_LINE>.
*******
*******    ASSIGN COMPONENT 'ACCOUNT'      OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_ACCOUNT>.
*******    ASSIGN COMPONENT 'AUDITTRAIL'   OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_AUDITTRAIL>.
*******    ASSIGN COMPONENT 'ANALYSIS'     OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_ANALYSIS>.
*******    ASSIGN COMPONENT 'ENTITY'       OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_ENTITY>.
*******    ASSIGN COMPONENT 'CATEGORY'     OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_CATEGORY>.
*******    ASSIGN COMPONENT 'TIME'         OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_TIME>.
*******    ASSIGN COMPONENT 'PARTNER'      OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_PARTNER>.
*******    ASSIGN COMPONENT 'RPTCURRENCY'  OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_RPTCURRENCY>.
*******    ASSIGN COMPONENT 'SIGNEDDATA'   OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_SIGNEDDATA>.
*******
**************************************************************************************************
******************* SOURCE STRUCTURE ********************************************************
    LOOP AT CT_DATA ASSIGNING <LS_RESULT_REC>.

      ASSIGN COMPONENT 'RESOURCE_ACCT'  OF STRUCTURE <LS_RESULT_REC> TO <LS_RESOURCE_ACCT>.
      ASSIGN COMPONENT 'EMPLOYEE'       OF STRUCTURE <LS_RESULT_REC> TO <LS_EMPLOYEE>.
      ASSIGN COMPONENT 'AUDITTRAIL'     OF STRUCTURE <LS_RESULT_REC> TO <LS_AUDITTRAIL>.
      ASSIGN COMPONENT 'ENTITY'         OF STRUCTURE <LS_RESULT_REC> TO <LS_ENTITY>.
      ASSIGN COMPONENT 'CATEGORY'       OF STRUCTURE <LS_RESULT_REC> TO <LS_CATEGORY>.
      ASSIGN COMPONENT 'TIME'           OF STRUCTURE <LS_RESULT_REC> TO <LS_TIME>.
      ASSIGN COMPONENT 'RPTCURRENCY'    OF STRUCTURE <LS_RESULT_REC> TO <LS_RPTCURRENCY>.
      ASSIGN COMPONENT 'SIGNEDDATA'     OF STRUCTURE <LS_RESULT_REC> TO <LS_SIGNEDDATA>.

      CHECK <LS_AUDITTRAIL> = 'INPUT'  OR <LS_AUDITTRAIL> = 'INPUT_ITSCREDIT'  .

     <LS_TARGET_TIME>        = <LS_TIME>.
*Change 1 starts **********************************************
*     IF <LS_AUDITTRAIL> = 'INPUT_ITSCREDIT'.
*     <LS_TARGET_AUDITTRAIL>  =  'RESOURCE_ITSCREDIT'. "Write data onto RESOURCE_ITSCREDIT audittrail in IS model
*     ELSE.
     <LS_TARGET_AUDITTRAIL>  =  'RESOURCE_INPUT'. "If input - set to RESOURCE_INPUT
*     ENDIF.
*Change 1 ends   **********************************************
     <LS_TARGET_CATEGORY>    = <LS_CATEGORY>.
     <LS_TARGET_RPTCURRENCY> = <LS_RPTCURRENCY>.
     <LS_TARGET_SIGNEDDATA>  = <LS_SIGNEDDATA>.

     CLEAR: BS_TARGETACCT , FLOW_ANALYSIS , ITS_CC,
     PROFIT_CENTRE, ITS_PROFIT_CENTRE, EMP_PROFIT_CENTRE.

     READ TABLE LT_RESOURCE_ACCT INTO LS_RESOURCE_ACCT
     WITH KEY
     ID = <LS_RESOURCE_ACCT>
     BINARY SEARCH.
     IF SY-SUBRC = 0 AND ( LS_RESOURCE_ACCT-BS_TARGETACCT IS NOT INITIAL OR
       LS_RESOURCE_ACCT-FLOW_ANALYSIS IS NOT INITIAL ) .
       BS_TARGETACCT = LS_RESOURCE_ACCT-BS_TARGETACCT.
       FLOW_ANALYSIS = LS_RESOURCE_ACCT-FLOW_ANALYSIS.
       ELSE.
         BS_TARGETACCT = 'NO_BSTARGETACCT-' && <LS_RESOURCE_ACCT>.
         FLOW_ANALYSIS = 'NO_ANALYSIS_MATCH-' && <LS_RESOURCE_ACCT>.
         ENDIF.

     READ TABLE LT_EMPLOYEE INTO LS_EMPLOYEE
     WITH KEY
     ID = <LS_EMPLOYEE>
     BINARY SEARCH.
     IF SY-SUBRC = 0 AND LS_EMPLOYEE-ITSS_CC IS NOT INITIAL.
       ITS_CC         = LS_EMPLOYEE-ITSS_CC.
       READ TABLE LT_ENTITY INTO LS_ENTITY
       WITH KEY
       ID = ITS_CC.
       IF SY-SUBRC = 0.
         ITS_PROFIT_CENTRE = LS_ENTITY-PROFIT_CENTRE.
         ELSE.
           ITS_PROFIT_CENTRE = 'NO_PROFIT_CENTRE-' && ITS_CC  .
           ENDIF.

       ELSE.
         ITS_CC = 'NO_ITS_CC-' && <LS_EMPLOYEE>  .
         ENDIF.

     READ TABLE LT_EMPLOYEE INTO LS_EMPLOYEE
     WITH KEY
     ID = <LS_EMPLOYEE>
     BINARY SEARCH.
        IF SY-SUBRC = 0 AND LS_EMPLOYEE-PROFIT_CENTRE IS NOT INITIAL.
          EMP_PROFIT_CENTRE = LS_EMPLOYEE-PROFIT_CENTRE.
          ELSE.
            EMP_PROFIT_CENTRE = 'NONE' .
            CLEAR: ET_MESSAGE, L_LOG.
            L_LOG = <LS_EMPLOYEE> && 'EMPLOYEE has no PROFIT_CENTRE property maintained'.
            CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).
          ENDIF.


       READ TABLE LT_ENTITY INTO LS_ENTITY
       WITH KEY
       ID = <LS_ENTITY>.
       IF SY-SUBRC = 0.
         PROFIT_CENTRE = LS_ENTITY-PROFIT_CENTRE.
         ELSE.
           PROFIT_CENTRE = 'NO_PROFIT_CENTRE-' && <LS_ENTITY>  .
           ENDIF.

    "DATA MUST BE FROM STAT_ACCT OR RP000408
    READ TABLE LT_STATACCT TRANSPORTING NO FIELDS
    WITH KEY TABLE_LINE = <LS_RESOURCE_ACCT>.
    IF SY-SUBRC <> 0 AND <LS_RESOURCE_ACCT> <> 'RP000408'.
      CONTINUE.
      ENDIF.

CASE SUB_ID.

  WHEN 'NONE'.
"CHECK IF EMPLOYEE IS UNDER TOTAL_ITSSROLES
    READ TABLE LT_ITSSROLES TRANSPORTING NO FIELDS
    WITH KEY TABLE_LINE = <LS_EMPLOYEE>.
    IF SY-SUBRC = 0 AND <LS_RESOURCE_ACCT> <> 'RP000408'.
      "CHECK IF UNDER IS_ITSS_DEMAND
      READ TABLE LT_IS_ITSS_DEMAND TRANSPORTING NO FIELDS
      WITH KEY TABLE_LINE = <LS_RESOURCE_ACCT>.
      IF SY-SUBRC = 0.
        "THESE ARE UNDER IS_ITSS_DEMAND
        <LS_TARGET_ACCOUNT>  = <LS_RESOURCE_ACCT> .
        <LS_TARGET_ANALYSIS> = 'NO_ANALYSIS' .
        <LS_TARGET_PARTNER>  = 'I_' && EMP_PROFIT_CENTRE."ITS_PROFIT_CENTRE."'I_' && ITS_CC.
        <LS_TARGET_ENTITY>   = <LS_ENTITY>.
        COLLECT <TARGET_LINE> INTO <TARGET_DATA>.

        ELSE.
        "THESE ARE UNDER BS_ITSS_DEMAND
        <LS_TARGET_ACCOUNT>   = BS_TARGETACCT .
        <LS_TARGET_ANALYSIS>  = FLOW_ANALYSIS .
        <LS_TARGET_PARTNER>   = 'I_' && EMP_PROFIT_CENTRE."ITS_PROFIT_CENTRE."'I_' && ITS_CC.
        <LS_TARGET_ENTITY>    = <LS_ENTITY>.
        COLLECT <TARGET_LINE> INTO <TARGET_DATA>.

        ENDIF.

      ELSEIF SY-SUBRC <> 0 AND <LS_RESOURCE_ACCT> <> 'RP000408'."NOT CHILDREN OF TOTAL_ITSSROLES

        READ TABLE LT_IS_GLACCT TRANSPORTING NO FIELDS
        WITH KEY TABLE_LINE = <LS_RESOURCE_ACCT>.
        IF SY-SUBRC = 0.
          <LS_TARGET_ACCOUNT>   = <LS_RESOURCE_ACCT> .
          <LS_TARGET_ANALYSIS>  = 'NO_ANALYSIS' .
          <LS_TARGET_PARTNER>   = 'I_NONE' .
          <LS_TARGET_ENTITY>    = <LS_ENTITY>.
          COLLECT <TARGET_LINE> INTO <TARGET_DATA>.

          <LS_TARGET_ACCOUNT>   = BS_TARGETACCT .
          <LS_TARGET_ANALYSIS>  = FLOW_ANALYSIS .
          <LS_TARGET_PARTNER>   = 'I_NONE' .
          <LS_TARGET_ENTITY>    = <LS_ENTITY>.
          <LS_TARGET_SIGNEDDATA>  = <LS_SIGNEDDATA> * -1.
          COLLECT <TARGET_LINE> INTO <TARGET_DATA>.
          ENDIF.

        READ TABLE LT_BS_OTHACCT TRANSPORTING NO FIELDS
        WITH KEY TABLE_LINE = <LS_RESOURCE_ACCT>.
        IF SY-SUBRC = 0.
          <LS_TARGET_ACCOUNT>   = BS_TARGETACCT .
          <LS_TARGET_ANALYSIS>  = FLOW_ANALYSIS .
          <LS_TARGET_PARTNER>   = 'I_NONE' .
          <LS_TARGET_ENTITY>    = <LS_ENTITY>.
          "IF BS_TARGETACCT <> 'AC310000P'.
          IF <LS_RESOURCE_ACCT> = 'AC300000P' . "OR BS_TARGETACCT = 'AC310000P'.
            <LS_TARGET_SIGNEDDATA>  = <LS_SIGNEDDATA> * -1.
            ENDIF.
          COLLECT <TARGET_LINE> INTO <TARGET_DATA>.
          ENDIF.

        READ TABLE LT_BS_ICOTHACCT TRANSPORTING NO FIELDS
        WITH KEY TABLE_LINE = <LS_RESOURCE_ACCT>.
        IF SY-SUBRC = 0.
          <LS_TARGET_ACCOUNT>   = BS_TARGETACCT .
          <LS_TARGET_ANALYSIS>  = FLOW_ANALYSIS .
          <LS_TARGET_PARTNER>   = 'I_' && EMP_PROFIT_CENTRE."ITS_PROFIT_CENTRE."'I_' && ITS_CC.
          <LS_TARGET_ENTITY>    = <LS_ENTITY>.
          COLLECT <TARGET_LINE> INTO <TARGET_DATA>.
          ENDIF.

      ELSEIF <LS_RESOURCE_ACCT> = 'RP000408'.

        <LS_TARGET_ACCOUNT>   = <LS_RESOURCE_ACCT> .
        <LS_TARGET_ANALYSIS>  = 'NO_ANALYSIS' .
        <LS_TARGET_PARTNER>   = 'I_NONE' .
        <LS_TARGET_ENTITY>    = <LS_ENTITY>.
        COLLECT <TARGET_LINE> INTO <TARGET_DATA>.

      ENDIF.

WHEN 'ITS_C'.    "'ITS_CREDIT VIA DATA MANAGER'.
"CHECK IF EMPLOYEE IS UNDER TOTAL_ITSSROLES BECAUSE THIS PART IS ONLY VALID FOR ITSSROLES
    READ TABLE LT_ITSSROLES TRANSPORTING NO FIELDS
    WITH KEY TABLE_LINE = <LS_EMPLOYEE>.
    IF SY-SUBRC = 0.
      "CHECK IF UNDER IS_ITSS_DEMAND
      READ TABLE LT_IS_ITSS_DEMAND TRANSPORTING NO FIELDS
      WITH KEY TABLE_LINE = <LS_RESOURCE_ACCT>.
      IF SY-SUBRC = 0.
        <LS_TARGET_ACCOUNT>  = <LS_RESOURCE_ACCT> .
        <LS_TARGET_ANALYSIS> = 'NO_ANALYSIS' .
        <LS_TARGET_PARTNER>  = 'I_' && PROFIT_CENTRE."'I_' && ITS_CC.
        <LS_TARGET_AUDITTRAIL> = 'RESOURCE_ITSCREDIT'."Added 28.09.2017 - CHANGE 3 to split data
        <LS_TARGET_ENTITY>   = ITS_CC .
*Change 2 Remove Starts **************************************************************************
        <LS_TARGET_SIGNEDDATA> = <LS_TARGET_SIGNEDDATA> * -1. "28.09.2017 - change is back
*Change 2 Remove Ends**************************************************************************
*Change 2 Insert Starts **************************************************************************
        "<LS_TARGET_SIGNEDDATA> = <LS_TARGET_SIGNEDDATA>. "ITS Demand Accounts should not be flipped, they are flipped during creation originally already
*Change 2 Insert Ends **************************************************************************
        COLLECT <TARGET_LINE> INTO <TARGET_DATA>.
        ENDIF.
      "CHECK IF UNDER BS_ITSS_DEMAND
***      READ TABLE LT_BS_ITSS_DEMAND TRANSPORTING NO FIELDS
***      WITH KEY TABLE_LINE = <LS_RESOURCE_ACCT>.
***      IF SY-SUBRC = 0.
***        <LS_TARGET_ACCOUNT>  = BS_TARGETACCT .
***        <LS_TARGET_ANALYSIS> = FLOW_ANALYSIS .
***        <LS_TARGET_PARTNER>  = 'I_' && PROFIT_CENTRE."'I_' && ITS_CC.
***        <LS_TARGET_ENTITY>   = ITS_CC .
***        COLLECT <TARGET_LINE> INTO <TARGET_DATA>.
***        ENDIF.
        "CHECK IF UNDER BS_OTHACCT
        READ TABLE LT_BS_OTHACCT TRANSPORTING NO FIELDS
        WITH KEY TABLE_LINE = <LS_RESOURCE_ACCT>.
        IF SY-SUBRC = 0 AND BS_TARGETACCT = 'AC220000' .
          <LS_TARGET_ACCOUNT>   =  BS_TARGETACCT .
          <LS_TARGET_ANALYSIS>  =  FLOW_ANALYSIS .
          "<LS_TARGET_PARTNER>   =  'I_' && EMP_PROFIT_CENTRE."ITS_PROFIT_CENTRE."'I_' && ITS_CC.
          <LS_TARGET_PARTNER>  = 'I_' && PROFIT_CENTRE."'I_' && ITS_CC.
          <LS_TARGET_AUDITTRAIL> = 'RESOURCE_ITSCREDIT'."Added 28.09.2017 - CHANGE 3 to split data
          "<LS_TARGET_ENTITY>    =  <LS_ENTITY>.
          <LS_TARGET_ENTITY>   = ITS_CC."Added 28.09.2017 - CHANGE 3 to show AC22 on the correct partner and entity
****          "IF BS_TARGETACCT <> 'AC310000P'.
****          IF <LS_RESOURCE_ACCT> <> 'AC310000P'.
****            <LS_TARGET_SIGNEDDATA>  = <LS_SIGNEDDATA> * -1.
****            ENDIF.
          <LS_TARGET_SIGNEDDATA> = <LS_SIGNEDDATA> * -1. "Added 28.09.2017 - CHANGE 3 to show correct sign on Cash.
          COLLECT <TARGET_LINE> INTO <TARGET_DATA>.
          ENDIF.

      ENDIF.

WHEN OTHERS.


ENDCASE.

        ENDLOOP.

******************************************************************
***** READY TO WRITE BACK *******
    LO_TOOLS->WRITE_MODEL_DATA(
      EXPORTING
       TARGET_MODEL     = 'INC_STATEMENT'
       INPUT_DATA  = <TARGET_DATA>
        WRITE  = 'Y'
      IMPORTING
       ET_MESSAGE  = ET_MESSAGE
       "ET_ERROR_RECORDS = ET_ERROR_RECORDS
       ).

    CLEAR CT_DATA.
*********************************

  ENDMETHOD.
ENDCLASS.
