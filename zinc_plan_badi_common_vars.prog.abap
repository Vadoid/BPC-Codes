*&---------------------------------------------------------------------*
*&  Include           ZINC_PLAN_BADI_COMMON_VARS
*&---------------------------------------------------------------------*

*Structures
 DATA: BEGIN OF LS_TIME,
         ID           TYPE CHAR32,
         CURRMONTH    TYPE CHAR32,
         BUDMONTH     TYPE CHAR32,
         TIMEID       TYPE CHAR32,
         VAT_QLY      TYPE CHAR32,
         COPYFORECAST TYPE CHAR32,
       END OF LS_TIME.

 DATA: BEGIN OF LS_EMPLOYEE,
         ID              TYPE CHAR32,
         RESOURCE_TYPE   TYPE CHAR32,
         EMPLOYEE_STATUS TYPE CHAR1,
         ITSS_CC         TYPE CHAR20,
         PROFIT_CENTRE   TYPE CHAR20,
       END OF LS_EMPLOYEE.

    DATA: BEGIN OF LS_RESOURCE_ACCT,
            ID            TYPE CHAR32,
            BS_TARGETACCT TYPE CHAR8,
            IS_TARGETACCT TYPE CHAR8,
            FLOW_ANALYSIS TYPE CHAR20,
          END OF LS_RESOURCE_ACCT.

 DATA: BEGIN OF LS_ACCOUNT,
         ID          TYPE CHAR32,
         DIMLIST     TYPE CHAR32,
         SHIFT_VALUE TYPE CHAR32,
       END OF LS_ACCOUNT.

 DATA: BEGIN OF LS_DRIVERS,
         ACCOUNT    TYPE CHAR32,
         ANALYSIS   TYPE CHAR32,
         AUDITTRAIL TYPE CHAR32,
         CATEGORY   TYPE CHAR32,
         ENTITY     TYPE CHAR32,
         PARTNER    TYPE CHAR32,
         SIGNEDDATA TYPE UJ_SDATA,
       END OF LS_DRIVERS.

 DATA: BEGIN OF LS_ENTITY,
         ID            TYPE CHAR32,
         COMPANY_CODE  TYPE CHAR32,
         PROFIT_CENTRE TYPE CHAR32,
         ECCSTATUS_ID  TYPE CHAR2,
       END OF LS_ENTITY.

 DATA: BEGIN OF LS_PROFITCTR,
         PROFIT_CENTRE TYPE CHAR32,
       END OF LS_PROFITCTR.

 DATA: BEGIN OF LS_PARTNER,
         ID           TYPE CHAR32,
         COMPANY_CODE TYPE CHAR32,
       END OF LS_PARTNER.

 DATA: BEGIN OF LS_PROFIT_CENTRE,
         ID           TYPE CHAR32,
         COMPANY_CODE TYPE CHAR32,
         ECCSTATUS_ID TYPE CHAR2,
       END OF LS_PROFIT_CENTRE.

*End of structures



** Param Related Variables****************************

 DATA:            LT_TIMESCOPE      LIKE SORTED TABLE OF LS_TIME WITH UNIQUE KEY ID,
                  LT_TIMEQRTLY      LIKE SORTED TABLE OF LS_TIME WITH UNIQUE KEY ID,
                  LS_TIMEQRTLY      LIKE LS_TIME,
                  LT_SHIFT_ACCOUNTS LIKE SORTED TABLE OF LS_ACCOUNT WITH UNIQUE KEY ID,
                  LS_SHIFT_ACCOUNTS LIKE LS_ACCOUNT,
                  LT_PROFIT_CENTRE  LIKE SORTED TABLE OF LS_PROFIT_CENTRE WITH UNIQUE KEY ID,
                  LT_DRIVERS        LIKE TABLE OF LS_DRIVERS,
                  LT_ACCOUNT        LIKE SORTED TABLE OF LS_ACCOUNT WITH UNIQUE KEY ID,
                  LT_ENTITY         LIKE SORTED TABLE OF LS_ENTITY WITH UNIQUE KEY ID,
                  LT_ENTITY_CV      LIKE SORTED TABLE OF LS_ENTITY WITH UNIQUE KEY ID,
                  LT_PROFITCTR      LIKE STANDARD TABLE OF LS_PROFITCTR ,
                  LT_EMPLOYEE       LIKE SORTED TABLE OF LS_EMPLOYEE WITH UNIQUE KEY ID,
                  LT_RESOURCE_ACCT  LIKE SORTED TABLE OF LS_RESOURCE_ACCT WITH UNIQUE KEY ID,
                  LT_PARTNER        LIKE SORTED TABLE OF LS_PARTNER WITH UNIQUE KEY ID,
                  LT_TARGET_DATA    TYPE HASHED TABLE OF UJP_CALC_ACCOUNT WITH UNIQUE KEY  CALC_ID  SEQ
                     ACCOUNT
                     DEST_ACCOUNT
                     SUBTABLES
                     DEST_SUBTABLES
                     INVERT_SIGN,
                  LS_TARGET_DATA    LIKE LINE OF LT_TARGET_DATA,
                  LO_TOOLS          TYPE REF TO ZCL_BPC_TOOLS_CLASSIC, "INSTANCE REFERENCE
                  LT_SEL            TYPE UJ0_T_SEL,
                  LS_SEL            TYPE UJ0_S_SEL,
                  LS_MESSAGE        TYPE UJ0_S_MESSAGE,
                  LT_DRIVER_SEL     TYPE UJ0_T_SEL,
                  LR_DATA           TYPE REF TO DATA,
                  LRR_DATA          TYPE REF TO DATA,
                  LR_TARGET_LINE    TYPE REF TO DATA,
                  FIRST_PERIOD      TYPE UJ_DIM_MEMBER,
                  LAST_PERIOD       TYPE UJ_DIM_MEMBER,
                  CURRENT_PERIOD    TYPE UJ_DIM_MEMBER,
                  PREVIOUS_PERIOD   TYPE UJ_DIM_MEMBER,
                  FCST_SNAPSHOT     TYPE STRING,
                  L_LOG             TYPE STRING,
                  COUNTER           TYPE UJ_SMALLINT,
                  MASTER_DATA       TYPE REF TO DATA,
                  LS_TIMEID         TYPE STRING,
                  LS_FLOW TYPE STRING,
                  LS_SIGNEDDATA     TYPE  UJ_SDATA,
                  YEAR              TYPE CHAR4,
                  MONTH             TYPE CHAR2,
                  SOURCE_PERIOD     TYPE UJ_DIM_MEMBER,
                  TARGET_PERIOD     TYPE UJ_DIM_MEMBER,
                  OFFSET            TYPE UJ_SDATA,
                  SOURCE_ACCOUNT    TYPE CHAR32,
                  SOURCE_ANALYSIS   TYPE CHAR32,
                  TAX_DRIVER        TYPE UJ_SDATA,
                  SOURCE_SIGNEDDATA TYPE  UJ_SDATA,
                  ANALYSIS          TYPE UJA_T_DIM_MEMBER,
                  ENTITY            TYPE UJA_T_DIM_MEMBER,
                  WA_MEMBER         TYPE UJ_DIM_MEMBER,
                  FLOWS             TYPE UJA_T_DIM_MEMBER,
                  ACCOUNTS          TYPE UJA_T_DIM_MEMBER,
                  FLAG_ACCOUNTS     TYPE UJA_T_DIM_MEMBER,
                  STAT_ACCOUNTS     TYPE UJA_T_DIM_MEMBER,
                  "Account constants could be adjusted as required
                  ACCOUNT_FLAG_NODE TYPE UJ_DIM_MEMBER VALUE 'TOTAL_FLAGACCT', "Constant for flag accounts
                  ACCOUNT_STAT_NODE TYPE UJ_DIM_MEMBER VALUE 'TOTAL_STATACCT', "Constant for stat accounts
                  TVARVC_NAME       TYPE RVARI_VNAM,
                  CALLER_ID_PARAM   TYPE STRING VALUE 'CALLER_ID',
                  SUB_ID_PARAM      TYPE STRING VALUE 'SUB_ID',
                  LS_PARAM          TYPE UJK_S_SCRIPT_LOGIC_HASHENTRY,
                  LV_PARAM          TYPE CHAR20,
                  CALLER_ID         TYPE STRING, "id of the calling model or specific calculation
                  CV_ACCOUNTS       TYPE UJK_S_CV,
                  CV_PROFIT_CENTRE  TYPE UJK_S_CV,
                  CV_PARTNER        TYPE UJK_S_CV,
                  CV_ENTITY         TYPE UJK_S_CV,
                  CV_TIME           TYPE UJK_S_CV,
                  CV_AUDITTRAIL     TYPE UJK_S_CV,
                  CV_CATEGORY       TYPE UJK_S_CV,
                  LT_IS_GLACCT      TYPE UJA_T_DIM_MEMBER,
                  LT_IS_ITSS_DEMAND TYPE UJA_T_DIM_MEMBER,
                  LT_BS_ITSS_DEMAND    TYPE UJA_T_DIM_MEMBER,
                  LT_IS_ITSS_SUPPLY    TYPE UJA_T_DIM_MEMBER,
                  LT_BS_ITSS_SUPPLY    TYPE UJA_T_DIM_MEMBER,
                  LT_BS_ICOTHACCT   TYPE UJA_T_DIM_MEMBER,
                  LT_BS_OTHACCT     TYPE UJA_T_DIM_MEMBER,
                  LT_BS_COSTACCT    TYPE UJA_T_DIM_MEMBER,
                  LT_STATACCT       TYPE UJA_T_DIM_MEMBER,
                  BS_TARGETACCT     TYPE UJ_DIM_MEMBER,
                  FLOW_ANALYSIS     TYPE UJ_DIM_MEMBER,
                  ITS_CC            TYPE UJ_DIM_MEMBER,
                  LT_ITSSROLES      TYPE UJA_T_DIM_MEMBER, "CONTAINS CHILDREN OF TOTAL_ITSSROLES
                  PROFIT_CENTRE     TYPE UJ_DIM_MEMBER,
                  EMP_PROFIT_CENTRE TYPE UJ_DIM_MEMBER,
                  ITS_PROFIT_CENTRE     TYPE UJ_DIM_MEMBER,
                  VAR_AUDITTRAIL    TYPE UJ_DIM_MEMBER,
                  LT_CV_ENTITY     TYPE UJA_T_DIM_MEMBER .


*************** FOR TOOLS **********************************************
 DATA: LO_APPL_MGR            TYPE REF TO IF_UJA_APPLICATION_MANAGER,
       LS_APPLICATION         TYPE UJA_S_APPLICATION,
       LS_DIMENSIONS          TYPE UJA_S_DIMENSION,
       "Constants for status **************
       ENTITY_STATUS_PROPERTY TYPE STRING,
       RESOURCE_STATUS        TYPE STRING VALUE 'EMPLOYEE_STATUS',
       CASHFLOW_STATUS        TYPE STRING VALUE 'ECCSTATUS_ID',
       INC_STATEMENT_STATUS   TYPE STRING VALUE 'ECCSTATUS_ID',
       "*********************************
       ENTITY_DIM             TYPE UJ_DIM_NAME.
*************************************************************************


 FIELD-SYMBOLS: <DIMENSION_DATA>          TYPE STANDARD TABLE,
                <TARGET_DATA>             TYPE STANDARD TABLE,
                <FX_DATA>                 TYPE STANDARD TABLE,
                <TARGET_LINE>             TYPE ANY,
                <CT_DATA>                 TYPE STANDARD TABLE,
                <LS_ASSET_CLASS>          TYPE ANY,
                <LS_AUDITTRAIL>           TYPE ANY,
                <LS_FLOW>                 TYPE ANY,
                <LS_PROFIT_CENTRE>        TYPE ANY,
                <LS_RPTCURRENCY>          TYPE ANY,
                <LS_RESOURCE_ACCT>        TYPE ANY,
                <LS_TIME>                 TYPE ANY,
                <LS_ACCOUNT>              TYPE ANY,
                <LS_ANALYSIS>             TYPE ANY,
                <LS_CATEGORY>             TYPE ANY,
                <LS_ENTITY>               TYPE ANY,
                <LS_EMPLOYEE>             TYPE ANY,
                <LS_PARTNER>              TYPE ANY,
                <LS_SIGNEDDATA>           TYPE ANY,
                <LS_TARGET_TIME>          TYPE ANY,
                <LS_TRAGET_FLOW>          TYPE ANY,
                <LS_TARGET_ACCOUNT>       TYPE ANY,
                <LS_TARGET_AUDITTRAIL>    TYPE ANY,
                <LS_TARGET_FLOW>          TYPE ANY,
                <LS_TARGET_PROFIT_CENTRE> TYPE ANY,
                <LS_TARGET_ANALYSIS>      TYPE ANY,
                <LS_TARGET_CATEGORY>      TYPE ANY,
                <LS_TARGET_ENTITY>        TYPE ANY,
                <LS_TARGET_RPTCURRENCY>   TYPE ANY,
                <LS_TARGET_PARTNER>       TYPE ANY,
                <LS_TARGET_SIGNEDDATA>    TYPE ANY.


********************* DATA VARIABLES *************************
 DATA: LT_FINAL      TYPE REF TO DATA,
       LS_REC        TYPE REF TO DATA,
       LS_RESULT_REC TYPE REF TO DATA.

 FIELD-SYMBOLS: <LT_FINAL>      TYPE STANDARD TABLE,
                <LS_REC>        TYPE ANY,
                <LS_RESULT_REC> TYPE ANY.
**************************************************************
TYPES: BEGIN OF LS_CASHFLOW,
       ACCOUNT       TYPE CHAR32,
       AUDITTRAIL    TYPE CHAR32,
       CATEGORY      TYPE CHAR32,
       FLOW          TYPE CHAR32,
       PARTNER       TYPE CHAR32,
       PROFIT_CENTRE TYPE CHAR32,
       RPTCURRENCY   TYPE CHAR32,
       TIME          TYPE CHAR32,
       SIGNEDDATA    TYPE UJ_SDATA,
       END OF LS_CASHFLOW.
DATA: LT_CASHFLOW TYPE STANDARD TABLE OF LS_CASHFLOW,
      WA_CASHFLOW TYPE LS_CASHFLOW.
FIELD-SYMBOLS <LS_CASHFLOW> TYPE LS_CASHFLOW.

DATA: FLAG1 TYPE CHAR1, FLAG2 TYPE CHAR1, FLAG3 TYPE CHAR1.

TYPES: BEGIN OF LS_INC_STATEMENT,
       ACCOUNT       TYPE CHAR32,
       ANALYSIS      TYPE CHAR32,
       AUDITTRAIL    TYPE CHAR32,
       CATEGORY      TYPE CHAR32,
       ENTITY        TYPE CHAR32,
       PARTNER       TYPE CHAR32,
       RPTCURRENCY   TYPE CHAR32,
       TIME          TYPE CHAR32,
       SIGNEDDATA    TYPE UJ_SDATA,
       END OF LS_INC_STATEMENT.
DATA: LT_INC_STATEMENT TYPE STANDARD TABLE OF LS_INC_STATEMENT,
      LT_INC_STATEMENT0 TYPE STANDARD TABLE OF LS_INC_STATEMENT,
      WA_INC_STATEMENT TYPE LS_INC_STATEMENT.
FIELD-SYMBOLS <LS_INC_STATEMENT> TYPE LS_INC_STATEMENT.
