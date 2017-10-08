class ZCL_BPC_MODEL_TO_MODEL definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_UJ_CUSTOM_LOGIC .
protected section.
private section.
ENDCLASS.



CLASS ZCL_BPC_MODEL_TO_MODEL IMPLEMENTATION.


  method IF_UJ_CUSTOM_LOGIC~CLEANUP.
  endmethod.


METHOD IF_UJ_CUSTOM_LOGIC~EXECUTE.
*&---------------------------------------------------------------------*
*&  Class           ZCL_BPC_MODEL_TO_MODEL
*&---------------------------------------------------------------------*
****************************************************************************
* Program type....:
* Author...........:
* Date.............:
* ----------------------------------------------------------------------
* Description......:
*
*
****************************************************************************
* Change Log
****************************************************************************
* Changed on:                 By:
* Description:
****************************************************************************

 DATA: BEGIN OF LS_EA_ENTITY,
         ID                    TYPE CHAR20,
         IS_EA_PC              TYPE CHAR1,
         JV_EQUITYACCT_TARGET  TYPE CHAR20,
       END OF LS_EA_ENTITY.

  DATA: LT_EA_ENTITY LIKE TABLE OF LS_EA_ENTITY,
        LT_DIM_LIST            TYPE UJA_T_DIM_LIST,
        LS_DIM_NAME            TYPE UJ_DIM_NAME,
        DIM_MEMBER             TYPE UJ_DIM_MEMBER,
        N_CHAR(10)             TYPE C VALUE '0123456789',
        CURRENT_PERIOD_CONSOL  TYPE UJ_DIM_MEMBER,
        PREVIOUS_PERIOD_CONSOL TYPE UJ_DIM_MEMBER,
        FISCPER                TYPE /BI0/OIFISCPER,
        CONVERT                TYPE CHAR1,
        MEMBERS                TYPE UJA_T_DIM_MEMBER.


  INCLUDE ZINC_PLAN_BADI_COMMON_VARS. "Common Variables


***************************************************************
************** ASSIGN WAs *************************************
  CREATE DATA LT_FINAL LIKE CT_DATA.
  ASSIGN LT_FINAL->* TO <LT_FINAL>.
  CREATE DATA LS_RESULT_REC LIKE LINE OF CT_DATA.
  ASSIGN LS_RESULT_REC->* TO <LS_RESULT_REC>.
  CREATE DATA LS_REC LIKE LINE OF CT_DATA.
  ASSIGN LS_REC->* TO <LS_REC>.

  ASSIGN COMPONENT 'ACCOUNT' OF STRUCTURE <LS_RESULT_REC> TO <LS_ACCOUNT>.
  ASSIGN COMPONENT 'FLOW' OF STRUCTURE <LS_RESULT_REC> TO <LS_FLOW>.
  ASSIGN COMPONENT 'AUDITTRAIL' OF STRUCTURE <LS_RESULT_REC> TO <LS_AUDITTRAIL>.
  ASSIGN COMPONENT 'PROFIT_CENTRE' OF STRUCTURE <LS_RESULT_REC> TO <LS_PROFIT_CENTRE>.
  ASSIGN COMPONENT 'CATEGORY' OF STRUCTURE <LS_RESULT_REC> TO <LS_CATEGORY>.
  ASSIGN COMPONENT 'TIME' OF STRUCTURE <LS_RESULT_REC> TO <LS_TIME>.
  ASSIGN COMPONENT 'PARTNER' OF STRUCTURE <LS_RESULT_REC> TO <LS_PARTNER>.
  ASSIGN COMPONENT 'RPTCURRENCY' OF STRUCTURE <LS_RESULT_REC> TO <LS_RPTCURRENCY>.
  ASSIGN COMPONENT 'SIGNEDDATA' OF STRUCTURE <LS_RESULT_REC> TO <LS_SIGNEDDATA>.
*****************************************************************************************

*** SET TOOLS INSTANCE *****
  CREATE OBJECT LO_TOOLS
    EXPORTING
      I_ENV_ID = 'SOURCE_ENVIRONMENT' "REPLACE WITH SOURCE ENVIRONMENT
      I_MOD_ID = 'SOURCE_MODEL'. "REPLACE WITH SOURCE MODEL

*************** GET Source MODEL **********

  LO_TOOLS->WRITE_MODEL_DATA(
  EXPORTING
    TARGET_MODEL  = 'SOURCE_MODEL' "REPLACE WITH SOURCE MODEL
    WRITE       = 'N'
  IMPORTING
    LT_DIM_LIST = LT_DIM_LIST
    LR_DATA     = LR_DATA ).
******************************************************

  ASSIGN LR_DATA->* TO <TARGET_DATA>.
  CREATE DATA LR_TARGET_LINE LIKE LINE OF <TARGET_DATA>.
  ASSIGN LR_TARGET_LINE->* TO <TARGET_LINE>.

**** Read EA Entity ***************************
        CLEAR: LT_SEL, LS_SEL.

        LS_SEL-DIMENSION = 'ENTITY'.
        LS_SEL-ATTRIBUTE = 'CALC'.
        LS_SEL-SIGN = 'I'.
        LS_SEL-OPTION = 'EQ'.
        LS_SEL-LOW = 'N'.
        APPEND LS_SEL TO LT_SEL.

        LO_TOOLS->READ_MASTER_DATA(
          EXPORTING
            I_DIMENSION_ID   = 'ENTITY'
          IMPORTING
            OUTPUT_R_DATA = MASTER_DATA
          CHANGING
            LT_SEL        = LT_SEL ).

        ASSIGN MASTER_DATA->* TO <DIMENSION_DATA>.
        MOVE-CORRESPONDING <DIMENSION_DATA> TO LT_EA_ENTITY.

**** End Read EA Entity ***********************


  CLEAR: LT_SEL, LS_SEL.
*********************************
  LS_SEL-ATTRIBUTE = 'ID'.
  LS_SEL-SIGN = 'I'.
  LS_SEL-OPTION = 'EQ'.
*********************************

*Check if conversion of data required
  READ TABLE IT_PARAM WITH KEY HASHKEY = 'CONVERT' INTO LS_PARAM.
  CONVERT = LS_PARAM-HASHVALUE.
****************************************************************


  LOOP AT LT_DIM_LIST INTO LS_DIM_NAME.

    CLEAR LS_PARAM.

    READ TABLE IT_PARAM WITH KEY HASHKEY = LS_DIM_NAME INTO LS_PARAM.
    LS_SEL-DIMENSION = LS_DIM_NAME.

    IF LS_PARAM-HASHVALUE IS INITIAL.

      CONTINUE.
    ELSE.
      DIM_MEMBER = LS_PARAM-HASHVALUE.
    ENDIF.


    IF DIM_MEMBER = '<ALL>'.

      CONTINUE.
*        LS_SEL-LOW = ''.
*        APPEND LS_SEL TO LT_SEL.

    ELSEIF DIM_MEMBER(3) = 'BAS'.

      REPLACE ALL OCCURRENCES OF 'BAS(' IN DIM_MEMBER WITH ''.
      REPLACE ALL OCCURRENCES OF ')' IN DIM_MEMBER WITH ''.

      TRY.
          LO_TOOLS->GET_PARENT_MD(
          EXPORTING
            PARENT_MBR  = DIM_MEMBER
            I_DIMENSION_ID = LS_DIM_NAME
            RECEIVING
            OUTPUT_DATA = MEMBERS ).

        CATCH CX_UJ_BI_FATAL_ERROR.

         MESSAGE e008(ZCL_BPC_PLANNING_MSG) INTO L_LOG WITH 'SOURCE_ENVIRONMENT'. "REPLACE WITH SOURCE ENVIRONMENT
         CL_UJK_LOGGER=>LOG( I_OBJECT = L_LOG ).
         RAISE EXCEPTION TYPE CX_UJ_CUSTOM_LOGIC.

      ENDTRY.


      LOOP AT MEMBERS INTO WA_MEMBER.
        LS_SEL-LOW = WA_MEMBER.
        APPEND LS_SEL TO LT_SEL.
      ENDLOOP.

    ELSE.

      IF LS_DIM_NAME = 'TIME'.

      IF CONVERT = 'Y' AND DIM_MEMBER+5(2) <> '01'.

          LO_TOOLS->PERIOD_OFFSET(
          EXPORTING
            IMPORT_PERIOD = DIM_MEMBER
            OFFSET        = -1
            RECEIVING
            EXPORT_PERIOD = PREVIOUS_PERIOD ).

          FISCPER = PREVIOUS_PERIOD(4) && 0 && PREVIOUS_PERIOD+5(2).
          LS_SEL-LOW = ZCL_TIME=>CONVERT_BW_TIME_TO_BPC( FISCPER ).
          PREVIOUS_PERIOD_CONSOL = LS_SEL-LOW.
          APPEND LS_SEL TO LT_SEL.

        ENDIF.

        FISCPER = DIM_MEMBER(4) && 0 && DIM_MEMBER+5(2).
        CURRENT_PERIOD = DIM_MEMBER.
        LS_SEL-LOW = ZCL_TIME=>CONVERT_BW_TIME_TO_BPC( FISCPER ).
        CURRENT_PERIOD_CONSOL = LS_SEL-LOW.
        APPEND LS_SEL TO LT_SEL.


      ELSEIF LS_DIM_NAME = 'ENTITY'. "Build list of entites to read with the correct EA PCs

      LOOP AT LT_EA_ENTITY INTO LS_EA_ENTITY.

        IF LS_EA_ENTITY-IS_EA_PC = 'Y'.
           LS_SEL-LOW = LS_EA_ENTITY-JV_EQUITYACCT_TARGET.
           APPEND LS_SEL TO LT_SEL.
        ELSE.

        LS_SEL-LOW = LS_EA_ENTITY-ID.
        APPEND LS_SEL TO LT_SEL.

        ENDIF.


      ENDLOOP.

      ELSE.


        LS_SEL-LOW = DIM_MEMBER.
        APPEND LS_SEL TO LT_SEL.

      ENDIF.


    ENDIF.

  ENDLOOP.


  LO_TOOLS->READ_MODEL_DATA(
  EXPORTING
    READ_MODEL  = 'SOURCE_MODEL'
*    IT_CV       =
    IT_SEL      = LT_SEL
  IMPORTING
    OUTPUT_DATA = <TARGET_DATA> ).


  LOOP AT <TARGET_DATA> INTO <TARGET_LINE>.

    ASSIGN COMPONENT 'ACCOUNT' OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_ACCOUNT>.
    ASSIGN COMPONENT 'CATEGORY' OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_CATEGORY>.
    ASSIGN COMPONENT 'INTERCO' OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_PARTNER>.
    ASSIGN COMPONENT 'FLOW' OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_FLOW>.
    ASSIGN COMPONENT 'ENTITY' OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_ENTITY>.
    ASSIGN COMPONENT 'CURRENCY' OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_RPTCURRENCY>.
    ASSIGN COMPONENT 'TIME' OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_TIME>.
    ASSIGN COMPONENT 'SIGNEDDATA' OF STRUCTURE <TARGET_LINE> TO <LS_TARGET_SIGNEDDATA>.

    REPLACE ALL OCCURRENCES OF 'C_' IN <LS_TARGET_ENTITY> WITH ''.

    IF <LS_TARGET_ENTITY>+5(1) NA N_CHAR.
      REPLACE ALL OCCURRENCES OF 'PC000' IN <LS_TARGET_ENTITY> WITH 'PC'.
    ENDIF.


    <LS_AUDITTRAIL> = 'BW_SAP'.
    <LS_FLOW> = <LS_TARGET_FLOW>.
    <LS_CATEGORY> = <LS_TARGET_CATEGORY>.
    <LS_PARTNER> = <LS_TARGET_PARTNER>.
    <LS_PROFIT_CENTRE> = <LS_TARGET_ENTITY>.
    <LS_RPTCURRENCY> = <LS_TARGET_RPTCURRENCY>.
    <LS_ACCOUNT> = 'AC' && <LS_TARGET_ACCOUNT>.


    IF CONVERT = 'Y' AND CURRENT_PERIOD_CONSOL+5(3) <> 'JAN'. "Do the YTD to periodic conversion
                                                              "Don't do for JAN


      IF <LS_TARGET_TIME> = CURRENT_PERIOD_CONSOL.
        <LS_TIME> = CURRENT_PERIOD.
        <LS_SIGNEDDATA> = <LS_TARGET_SIGNEDDATA>.
      ELSE.
        <LS_TIME> = CURRENT_PERIOD.
        <LS_SIGNEDDATA> = ( <LS_TARGET_SIGNEDDATA> * -1 ).
      ENDIF.

      COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.


    ELSE. "Don't do the conversion

      <LS_TIME> = CURRENT_PERIOD.
      <LS_SIGNEDDATA> = <LS_TARGET_SIGNEDDATA>.
      COLLECT <LS_RESULT_REC> INTO <LT_FINAL>.

    ENDIF.
  ENDLOOP.


  CLEAR CT_DATA.
  MOVE-CORRESPONDING <LT_FINAL> TO CT_DATA.


ENDMETHOD.


  method IF_UJ_CUSTOM_LOGIC~INIT.
  endmethod.
ENDCLASS.
