&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

def var d-num1    as deci no-undo.
def var d-num2    as deci no-undo.
def var c-oper    as char no-undo.
def var c-result  as char no-undo.
def var c-result2 as char no-undo.


def temp-table tt-hist
    field d-numero1 as deci label "Numero Digitado"
    field c-oper    as char label "Operação"                                       
    field d-numero2 as deci label "Numero Digitado 2"
    field c-resut as char label "Resultado".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fl-display bt-1 bt-2 bt-3 bt-mais bt-4 bt-5 ~
bt-6 bt-menos bt-7 bt-8 bt-9 bt-div bt-limpa bt-0 bt-igual bt-multi bt-ht 
&Scoped-Define DISPLAYED-OBJECTS fl-display 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-0 
     LABEL "0" 
     SIZE 9 BY 1.83.

DEFINE BUTTON bt-1 
     LABEL "1" 
     SIZE 9 BY 1.83.

DEFINE BUTTON bt-2 
     LABEL "2" 
     SIZE 9 BY 1.83.

DEFINE BUTTON bt-3 
     LABEL "3" 
     SIZE 9 BY 1.83.

DEFINE BUTTON bt-4 
     LABEL "4" 
     SIZE 9 BY 1.83.

DEFINE BUTTON bt-5 
     LABEL "5" 
     SIZE 9 BY 1.83.

DEFINE BUTTON bt-6 
     LABEL "6" 
     SIZE 9 BY 1.83.

DEFINE BUTTON bt-7 
     LABEL "7" 
     SIZE 9 BY 1.83.

DEFINE BUTTON bt-8 
     LABEL "8" 
     SIZE 9 BY 1.83.

DEFINE BUTTON bt-9 
     LABEL "9" 
     SIZE 9 BY 1.83.

DEFINE BUTTON bt-div 
     LABEL "/" 
     SIZE 9 BY 1.83.

DEFINE BUTTON bt-ht 
     LABEL "History" 
     SIZE 30 BY 1.3.

DEFINE BUTTON bt-igual 
     LABEL "=" 
     SIZE 9 BY 1.83.

DEFINE BUTTON bt-limpa 
     LABEL "CE" 
     SIZE 9 BY 1.83.

DEFINE BUTTON bt-mais 
     LABEL "+" 
     SIZE 9 BY 1.83.

DEFINE BUTTON bt-menos 
     LABEL "-" 
     SIZE 9 BY 1.83.

DEFINE BUTTON bt-multi 
     LABEL "*" 
     SIZE 9 BY 1.83.

DEFINE VARIABLE fl-display AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 60 BY 2.61 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fl-display AT ROW 2.04 COL 11 NO-LABEL WIDGET-ID 2
     bt-1 AT ROW 5.17 COL 11 WIDGET-ID 4
     bt-2 AT ROW 5.17 COL 22 WIDGET-ID 48
     bt-3 AT ROW 5.17 COL 32 WIDGET-ID 40
     bt-mais AT ROW 5.17 COL 46 WIDGET-ID 26
     bt-4 AT ROW 7.26 COL 11 WIDGET-ID 54
     bt-5 AT ROW 7.26 COL 22 WIDGET-ID 46
     bt-6 AT ROW 7.26 COL 32 WIDGET-ID 6
     bt-menos AT ROW 7.26 COL 46 WIDGET-ID 8
     bt-7 AT ROW 9.35 COL 11 WIDGET-ID 52
     bt-8 AT ROW 9.35 COL 22 WIDGET-ID 44
     bt-9 AT ROW 9.35 COL 32 WIDGET-ID 38
     bt-div AT ROW 9.35 COL 46 WIDGET-ID 32
     bt-limpa AT ROW 11.44 COL 11 WIDGET-ID 50
     bt-0 AT ROW 11.44 COL 22 WIDGET-ID 42
     bt-igual AT ROW 11.44 COL 32 WIDGET-ID 36
     bt-multi AT ROW 11.44 COL 46 WIDGET-ID 34
     bt-ht AT ROW 13.52 COL 11 WIDGET-ID 58
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 71.8 BY 14 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 24.35
         WIDTH              = 126
         MAX-HEIGHT         = 24.35
         MAX-WIDTH          = 126
         VIRTUAL-HEIGHT     = 24.35
         VIRTUAL-WIDTH      = 126
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN fl-display IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN 
       fl-display:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-0
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-0 C-Win
ON CHOOSE OF bt-0 IN FRAME DEFAULT-FRAME /* 0 */
DO:
   run pi-display(input self:label).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-1 C-Win
ON CHOOSE OF bt-1 IN FRAME DEFAULT-FRAME /* 1 */
DO:
  run pi-display(input self:label).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-2 C-Win
ON CHOOSE OF bt-2 IN FRAME DEFAULT-FRAME /* 2 */
DO:
   run pi-display(input self:label).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-3 C-Win
ON CHOOSE OF bt-3 IN FRAME DEFAULT-FRAME /* 3 */
DO:
   run pi-display(input self:label).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-4 C-Win
ON CHOOSE OF bt-4 IN FRAME DEFAULT-FRAME /* 4 */
DO:
   run pi-display(input self:label).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-5 C-Win
ON CHOOSE OF bt-5 IN FRAME DEFAULT-FRAME /* 5 */
DO:
   run pi-display(input self:label).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-6 C-Win
ON CHOOSE OF bt-6 IN FRAME DEFAULT-FRAME /* 6 */
DO:
   run pi-display(input self:label).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-7 C-Win
ON CHOOSE OF bt-7 IN FRAME DEFAULT-FRAME /* 7 */
DO:
   run pi-display(input self:label).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-8 C-Win
ON CHOOSE OF bt-8 IN FRAME DEFAULT-FRAME /* 8 */
DO:
   run pi-display(input self:label).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-9 C-Win
ON CHOOSE OF bt-9 IN FRAME DEFAULT-FRAME /* 9 */
DO:
   run pi-display(input self:label).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-div
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-div C-Win
ON CHOOSE OF bt-div IN FRAME DEFAULT-FRAME /* / */
DO:
        assign c-oper = '/'.
    run pi-opera.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ht
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ht C-Win
ON CHOOSE OF bt-ht IN FRAME DEFAULT-FRAME /* History */
DO:
  run pi-hist.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-igual
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-igual C-Win
ON CHOOSE OF bt-igual IN FRAME DEFAULT-FRAME /* = */
DO:
    run pi-resu.
    //run pi-opera(input self:label).
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-limpa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-limpa C-Win
ON CHOOSE OF bt-limpa IN FRAME DEFAULT-FRAME /* CE */
DO:
  assign fl-display:screen-value = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-mais
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-mais C-Win
ON CHOOSE OF bt-mais IN FRAME DEFAULT-FRAME /* + */
DO:
    assign c-oper = '+'.
    run pi-opera.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-menos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-menos C-Win
ON CHOOSE OF bt-menos IN FRAME DEFAULT-FRAME /* - */
DO:
      assign c-oper = '-'.
    run pi-opera.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-multi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-multi C-Win
ON CHOOSE OF bt-multi IN FRAME DEFAULT-FRAME /* * */
DO:
      assign c-oper = '*'.
    run pi-opera.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY fl-display 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE fl-display bt-1 bt-2 bt-3 bt-mais bt-4 bt-5 bt-6 bt-menos bt-7 bt-8 
         bt-9 bt-div bt-limpa bt-0 bt-igual bt-multi bt-ht 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-display C-Win 
PROCEDURE pi-display :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input param d-num1 as deci no-undo.

do with  frame {&frame-name}:
    
    if length(fl-display:screen-value) >= 10 then return.
    
    assign fl-display:screen-value = fl-display:screen-value + string(d-num1).
            
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-hist C-Win 
PROCEDURE pi-hist :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 create tt-hist.
 assign tt-hist.d-numero1 = d-num2
        tt-hist.c-oper    = c-oper
        tt-hist.d-numero2 = d-num1
        tt-hist.c-resut   = c-result.

for each tt-hist no-lock:
    disp tt-hist
        with frame f1 center.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-opera C-Win 
PROCEDURE pi-opera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
do with frame {&frame-name }:

 
           
           assign  d-num2 = dec(fl-display:screen-value)
                 
                   fl-display:screen-value  = "".
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-resu C-Win 
PROCEDURE pi-resu :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

do with frame {&frame-name}:

 assign  d-num1 = dec(fl-display:screen-value).

  case c-oper:
      when "+"  then assign  c-result  = string (d-num2 + d-num1).
      when "-"  then assign  c-result  = string (d-num2 - d-num1).
      when "/"  then assign  c-result  = string (d-num2 / d-num1).
      when "*"  then assign  c-result  = string (d-num2 * d-num1).
  end case.

  assign  fl-display:screen-value = c-result
          c-result2 = c-result + c-oper
          c-result2 = c-result2. 
           
end.





END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

