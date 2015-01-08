/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Thursday, January 08, 2015     TIME: 1:52:50 PM
PROJECT: WarnerN_SAS_project_01_09_15
PROJECT PATH: P:\QAC\qac200\students\nwarner\Assignments\WarnerN_SAS_project_01_09_15.egp
---------------------------------------- */

/* Library assignment for Local.NATDATA */
Libname NATDATA BASE 'P:\QAC\qac200\students\nwarner' ;
/* Library assignment for Local.NATDATA */
Libname NATDATA BASE 'P:\QAC\qac200\students\nwarner' ;


/* Conditionally delete set of tables or views, if they exists          */
/* If the member does not exist, then no action is performed   */
%macro _eg_conditional_dropds /parmbuff;
	
   	%local num;
   	%local stepneeded;
   	%local stepstarted;
   	%local dsname;
	%local name;

   	%let num=1;
	/* flags to determine whether a PROC SQL step is needed */
	/* or even started yet                                  */
	%let stepneeded=0;
	%let stepstarted=0;
   	%let dsname= %qscan(&syspbuff,&num,',()');
	%do %while(&dsname ne);	
		%let name = %sysfunc(left(&dsname));
		%if %qsysfunc(exist(&name)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;

			%end;
				drop table &name;
		%end;

		%if %sysfunc(exist(&name,view)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;
			%end;
				drop view &name;
		%end;
		%let num=%eval(&num+1);
      	%let dsname=%qscan(&syspbuff,&num,',()');
	%end;
	%if &stepstarted %then %do;
		quit;
	%end;
%mend _eg_conditional_dropds;


/* Build where clauses from stored process parameters */

%macro _eg_WhereParam( COLUMN, PARM, OPERATOR, TYPE=S, MATCHALL=_ALL_VALUES_, MATCHALL_CLAUSE=1, MAX= , IS_EXPLICIT=0);

  %local q1 q2 sq1 sq2;
  %local isEmpty;
  %local isEqual isNotEqual;
  %local isIn isNotIn;
  %local isString;
  %local isBetween;

  %let isEqual = ("%QUPCASE(&OPERATOR)" = "EQ" OR "&OPERATOR" = "=");
  %let isNotEqual = ("%QUPCASE(&OPERATOR)" = "NE" OR "&OPERATOR" = "<>");
  %let isIn = ("%QUPCASE(&OPERATOR)" = "IN");
  %let isNotIn = ("%QUPCASE(&OPERATOR)" = "NOT IN");
  %let isString = (%QUPCASE(&TYPE) eq S or %QUPCASE(&TYPE) eq STRING );
  %if &isString %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%");
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq D or %QUPCASE(&TYPE) eq DATE %then 
  %do;
    %let q1=%str(%");
    %let q2=%str(%"d);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq T or %QUPCASE(&TYPE) eq TIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"t);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq DT or %QUPCASE(&TYPE) eq DATETIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"dt);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else
  %do;
    %let q1=;
    %let q2=;
	%let sq1=;
    %let sq2=;
  %end;
  
  %if "&PARM" = "" %then %let PARM=&COLUMN;

  %let isBetween = ("%QUPCASE(&OPERATOR)"="BETWEEN" or "%QUPCASE(&OPERATOR)"="NOT BETWEEN");

  %if "&MAX" = "" %then %do;
    %let MAX = &parm._MAX;
    %if &isBetween %then %let PARM = &parm._MIN;
  %end;

  %if not %symexist(&PARM) or (&isBetween and not %symexist(&MAX)) %then %do;
    %if &IS_EXPLICIT=0 %then %do;
		not &MATCHALL_CLAUSE
	%end;
	%else %do;
	    not 1=1
	%end;
  %end;
  %else %if "%qupcase(&&&PARM)" = "%qupcase(&MATCHALL)" %then %do;
    %if &IS_EXPLICIT=0 %then %do;
	    &MATCHALL_CLAUSE
	%end;
	%else %do;
	    1=1
	%end;	
  %end;
  %else %if (not %symexist(&PARM._count)) or &isBetween %then %do;
    %let isEmpty = ("&&&PARM" = "");
    %if (&isEqual AND &isEmpty AND &isString) %then
       &COLUMN is null;
    %else %if (&isNotEqual AND &isEmpty AND &isString) %then
       &COLUMN is not null;
    %else %do;
	   %if &IS_EXPLICIT=0 %then %do;
           &COLUMN &OPERATOR %unquote(&q1)&&&PARM%unquote(&q2)
	   %end;
	   %else %do;
	       &COLUMN &OPERATOR %unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2))
	   %end;
       %if &isBetween %then 
          AND %unquote(&q1)&&&MAX%unquote(&q2);
    %end;
  %end;
  %else 
  %do;
	%local emptyList;
  	%let emptyList = %symexist(&PARM._count);
  	%if &emptyList %then %let emptyList = &&&PARM._count = 0;
	%if (&emptyList) %then
	%do;
		%if (&isNotin) %then
		   1;
		%else
			0;
	%end;
	%else %if (&&&PARM._count = 1) %then 
    %do;
      %let isEmpty = ("&&&PARM" = "");
      %if (&isIn AND &isEmpty AND &isString) %then
        &COLUMN is null;
      %else %if (&isNotin AND &isEmpty AND &isString) %then
        &COLUMN is not null;
      %else %do;
	    %if &IS_EXPLICIT=0 %then %do;
            &COLUMN &OPERATOR (%unquote(&q1)&&&PARM%unquote(&q2))
	    %end;
		%else %do;
		    &COLUMN &OPERATOR (%unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2)))
		%end;
	  %end;
    %end;
    %else 
    %do;
       %local addIsNull addIsNotNull addComma;
       %let addIsNull = %eval(0);
       %let addIsNotNull = %eval(0);
       %let addComma = %eval(0);
       (&COLUMN &OPERATOR ( 
       %do i=1 %to &&&PARM._count; 
          %let isEmpty = ("&&&PARM&i" = "");
          %if (&isString AND &isEmpty AND (&isIn OR &isNotIn)) %then
          %do;
             %if (&isIn) %then %let addIsNull = 1;
             %else %let addIsNotNull = 1;
          %end;
          %else
          %do;		     
            %if &addComma %then %do;,%end;
			%if &IS_EXPLICIT=0 %then %do;
                %unquote(&q1)&&&PARM&i%unquote(&q2) 
			%end;
			%else %do;
			    %unquote(%nrstr(&sq1))&&&PARM&i%unquote(%nrstr(&sq2)) 
			%end;
            %let addComma = %eval(1);
          %end;
       %end;) 
       %if &addIsNull %then OR &COLUMN is null;
       %else %if &addIsNotNull %then AND &COLUMN is not null;
       %do;)
       %end;
    %end;
  %end;
%mend;

/* ---------------------------------- */
/* MACRO: enterpriseguide             */
/* PURPOSE: define a macro variable   */
/*   that contains the file system    */
/*   path of the WORK library on the  */
/*   server.  Note that different     */
/*   logic is needed depending on the */
/*   server type.                     */
/* ---------------------------------- */
%macro enterpriseguide;
%global sasworklocation;
%local tempdsn unique_dsn path;

%if &sysscp=OS %then %do; /* MVS Server */
	%if %sysfunc(getoption(filesystem))=MVS %then %do;
        /* By default, physical file name will be considered a classic MVS data set. */
	    /* Construct dsn that will be unique for each concurrent session under a particular account: */
		filename egtemp '&egtemp' disp=(new,delete); /* create a temporary data set */
 		%let tempdsn=%sysfunc(pathname(egtemp)); /* get dsn */
		filename egtemp clear; /* get rid of data set - we only wanted its name */
		%let unique_dsn=".EGTEMP.%substr(&tempdsn, 1, 16).PDSE"; 
		filename egtmpdir &unique_dsn
			disp=(new,delete,delete) space=(cyl,(5,5,50))
			dsorg=po dsntype=library recfm=vb
			lrecl=8000 blksize=8004 ;
		options fileext=ignore ;
	%end; 
 	%else %do; 
        /* 
		By default, physical file name will be considered an HFS 
		(hierarchical file system) file. 
		*/
		%if "%sysfunc(getoption(filetempdir))"="" %then %do;
			filename egtmpdir '/tmp';
		%end;
		%else %do;
			filename egtmpdir "%sysfunc(getoption(filetempdir))";
		%end;
	%end; 
	%let path=%sysfunc(pathname(egtmpdir));
    %let sasworklocation=%sysfunc(quote(&path));  
%end; /* MVS Server */
%else %do;
	%let sasworklocation = "%sysfunc(getoption(work))/";
%end;
%if &sysscp=VMS_AXP %then %do; /* Alpha VMS server */
	%let sasworklocation = "%sysfunc(getoption(work))";                         
%end;
%if &sysscp=CMS %then %do; 
	%let path = %sysfunc(getoption(work));                         
	%let sasworklocation = "%substr(&path, %index(&path,%str( )))";
%end;
%mend enterpriseguide;

%enterpriseguide

/* save the current settings of XPIXELS and YPIXELS */
/* so that they can be restored later               */
%macro _sas_pushchartsize(new_xsize, new_ysize);
	%global _savedxpixels _savedypixels;
	options nonotes;
	proc sql noprint;
	select setting into :_savedxpixels
	from sashelp.vgopt
	where optname eq "XPIXELS";
	select setting into :_savedypixels
	from sashelp.vgopt
	where optname eq "YPIXELS";
	quit;
	options notes;
	GOPTIONS XPIXELS=&new_xsize YPIXELS=&new_ysize;
%mend;

/* restore the previous values for XPIXELS and YPIXELS */
%macro _sas_popchartsize;
	%if %symexist(_savedxpixels) %then %do;
		GOPTIONS XPIXELS=&_savedxpixels YPIXELS=&_savedypixels;
		%symdel _savedxpixels / nowarn;
		%symdel _savedypixels / nowarn;
	%end;
%mend;

ODS PROCTITLE;
OPTIONS DEV=ACTIVEX;
GOPTIONS XPIXELS=0 YPIXELS=0;
FILENAME EGSRX TEMP;
ODS tagsets.sasreport13(ID=EGSRX) FILE=EGSRX
    STYLE=HtmlBlue
    STYLESHEET=(URL="file:///C:/Program%20Files/SASHome/SASEnterpriseGuide/6.1/Styles/HtmlBlue.css")
    NOGTITLE
    NOGFOOTNOTE
    GPATH=&sasworklocation
    ENCODING=UTF8
    options(rolap="on")
;

/*   START OF NODE: Assign Project Library (NATDATA)   */
%LET _CLIENTTASKLABEL='Assign Project Library (NATDATA)';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\nwarner\Assignments\WarnerN_SAS_project_01_09_15.egp';
%LET _CLIENTPROJECTNAME='WarnerN_SAS_project_01_09_15.egp';

GOPTIONS ACCESSIBLE;
LIBNAME NATDATA BASE "P:\QAC\qac200\students\nwarner" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: VarriablesAdult01.07.15   */
%LET _CLIENTTASKLABEL='VarriablesAdult01.07.15';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\nwarner\Assignments\WarnerN_SAS_project_01_09_15.egp';
%LET _CLIENTPROJECTNAME='WarnerN_SAS_project_01_09_15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(NATDATA.'NatVarriablesAdults01.07.15'n);

PROC SQL;
   CREATE TABLE NATDATA.'NatVarriablesAdults01.07.15'n(label="NatVarriablesAdults01.07.15") AS 
   SELECT t1.AMNURS12, 
          t1.AMDRC12, 
          t1.AMTOTC12, 
          t1.ERTOT12, 
          t1.IPDIS12, 
          t1.OBASST12, 
          t1.OBDRV12, 
          t1.OPDRV12, 
          t1.AMASST12, 
          t1.AGE12X, 
          t1.ANGIDX, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.CANCERDX, 
          t1.REGION12, 
          t1.CHDDX, 
          t1.AFDC12, 
          t1.EICRDT12, 
          t1.SAQELIG, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.MCDEV12, 
          t1.FAMINC12, 
          t1.SAQWT12F, 
          t1.INSURC12, 
          t1.MIDX, 
          t1.HIBPDX, 
          t1.INTVLANG, 
          t1.MARRY12X, 
          t1.DUPERSID, 
          t1.DIVDP12X, 
          t1.SSIP12X, 
          t1.TTLP12X, 
          t1.RACETHX, 
          t1.ADAPPT42, 
          t1.ADEXPL42, 
          t1.ADLIST42, 
          t1.ADTLHW42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADILWW42, 
          t1.ADRTWW42, 
          t1.ADFFRM42, 
          t1.ADRTCR42, 
          t1.ADSPEC42, 
          t1.ADFHLP42, 
          t1.ADHECR42, 
          t1.ADNSMK42, 
          t1.ADEGMC42, 
          t1.ADSPRF42, 
          t1.ADILCR42, 
          t1.ADNDCR42, 
          t1.ADDPRS42, 
          t1.ADINTR42, 
          t1.PHQ242, 
          t1.ADDRBP42, 
          t1.ADHOPE42, 
          t1.ADNERV42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADWRTH42, 
          t1.ADEFRT42, 
          t1.K6SUM42, 
          t1.ADCAPE42, 
          t1.ADDOWN42, 
          t1.ADNRGY42, 
          t1.ADSOCA42, 
          t1.ADMALS42, 
          t1.ADPALS42, 
          t1.ADPAIN42, 
          t1.ADMWLM42, 
          t1.ADPWLM42, 
          t1.ADOVER42, 
          t1.ADSMOK42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADINSA42, 
          t1.ADGENH42, 
          t1.ADINSB42, 
          t1.ADCLIM42, 
          t1.ADDAYA42, 
          t1.ADLANG42, 
          t1.ADRISK42, 
          t1.SFFLAG42, 
          t1.ADPRX42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.SEX, 
          t1.STRKDX, 
          t1.ERDEXP12, 
          t1.OBDEXP12, 
          t1.OBNEXP12, 
          t1.OPSEXP12, 
          t1.AMNEXP12, 
          t1.EDUYRDEG
      FROM EC100002.meps_fullyr_2012 t1
      WHERE t1.AGE12X >= 18
      ORDER BY t1.DUPERSID;
QUIT;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Code For VarriablesAdult01_07_15   */
%LET SYSLAST=NATDATA.NATVARRIABLESADULTS01.07.15;
%LET _CLIENTTASKLABEL='Code For VarriablesAdult01_07_15';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\nwarner\Assignments\WarnerN_SAS_project_01_09_15.egp';
%LET _CLIENTPROJECTNAME='WarnerN_SAS_project_01_09_15.egp';
%LET _SASPROGRAMFILE='C:\Users\nwarner\Desktop\nwarner\SASProgramCode\WarnerN_SAS_projectcode\Code For VarriablesAdult01_07_15.sas';

GOPTIONS ACCESSIBLE;

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Wednesday, January 07, 2015 at 2:38:19 PM
   By task: VarriablesAdult01.07.15

   Input Data: Local:NATDATA.NATVARRIABLESADULTS01.07.15
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsForNATVARRIABLESADUL);
TITLE "Data set attributes for sub set data" ;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=NATDATA.'NATVARRIABLESADULTS01.07.15'n ;

RUN;





GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;


/*   START OF NODE: One-Way Frequencies for 2012 adult MEPS Subset   */
%LET _CLIENTTASKLABEL='One-Way Frequencies for 2012 adult MEPS Subset';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\nwarner\Assignments\WarnerN_SAS_project_01_09_15.egp';
%LET _CLIENTPROJECTNAME='WarnerN_SAS_project_01_09_15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 08, 2015 at 1:52:13 PM
   By task: One-Way Frequencies for 2012 adult MEPS Subset

   Input Data: Local:NATDATA.NATVARRIABLESADULTS01.07.15
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:NATDATA.NATVARRIABLESADULTS01.07.15
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.DIVDP12X, T.SSIP12X, T.TTLP12X, T.RACETHX, T.ADAPPT42, T.ADEXPL42, T.ADLIST42, T.ADTLHW42, T.ADINST42, T.ADEZUN42, T.ADRESP42, T.ADPRTM42, T.ADILWW42, T.ADRTWW42, T.ADFFRM42, T.ADRTCR42, T.ADSPEC42, T.ADFHLP42, T.ADHECR42
		     , T.ADNSMK42, T.ADEGMC42, T.ADSPRF42, T.ADILCR42, T.ADNDCR42, T.ADDPRS42, T.ADINTR42, T.PHQ242, T.ADDRBP42, T.ADHOPE42, T.ADNERV42, T.ADREST42, T.ADSAD42, T.ADWRTH42, T.ADEFRT42, T.K6SUM42, T.ADCAPE42, T.ADDOWN42, T.ADNRGY42
		     , T.ADSOCA42, T.ADMALS42, T.ADPALS42, T.ADPAIN42, T.ADMWLM42, T.ADPWLM42, T.ADOVER42, T.ADSMOK42, T.ADCMPD42, T.ADCMPM42, T.ADCMPY42, T.ADINSA42, T.ADGENH42, T.ADINSB42, T.ADCLIM42, T.ADDAYA42, T.ADLANG42, T.ADRISK42, T.SFFLAG42
		     , T.ADPRX42, T.MCS42, T.PCS42, T.SEX, T.STRKDX, T.ERDEXP12, T.OBDEXP12, T.OBNEXP12, T.OPSEXP12, T.AMNEXP12, T.EDUYRDEG, T.AMNURS12, T.AMDRC12, T.AMTOTC12, T.ERTOT12, T.IPDIS12, T.OBASST12, T.OBDRV12, T.OPDRV12, T.AMASST12
		     , T.AGE12X, T.ANGIDX, T.ARTHDX, T.ASTHDX, T.CANCERDX, T.REGION12, T.CHDDX, T.AFDC12, T.EICRDT12, T.SAQELIG, T.EMPST31, T.EMPST42, T.EMPST53, T.MCDEV12, T.FAMINC12, T.SAQWT12F, T.INSURC12, T.MIDX, T.HIBPDX, T.INTVLANG
		     , T.MARRY12X
	FROM NATDATA.'NATVARRIABLESADULTS01.07.15'n(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results for MEPS 2012 adults (scaled down to 99 varriables, inc DUPERSID)";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
FOOTNOTE3 "by Nat Warner! =)";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES DIVDP12X / MISSPRINT  SCORES=TABLE;
	TABLES SSIP12X / MISSPRINT  SCORES=TABLE;
	TABLES TTLP12X / MISSPRINT  SCORES=TABLE;
	TABLES RACETHX / MISSPRINT  SCORES=TABLE;
	TABLES ADAPPT42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEXPL42 / MISSPRINT  SCORES=TABLE;
	TABLES ADLIST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADTLHW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEZUN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRESP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPRTM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADILWW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRTWW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADFFRM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRTCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSPEC42 / MISSPRINT  SCORES=TABLE;
	TABLES ADFHLP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADHECR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNSMK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEGMC42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSPRF42 / MISSPRINT  SCORES=TABLE;
	TABLES ADILCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNDCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDPRS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINTR42 / MISSPRINT  SCORES=TABLE;
	TABLES PHQ242 / MISSPRINT  SCORES=TABLE;
	TABLES ADDRBP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADHOPE42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNERV42 / MISSPRINT  SCORES=TABLE;
	TABLES ADREST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSAD42 / MISSPRINT  SCORES=TABLE;
	TABLES ADWRTH42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEFRT42 / MISSPRINT  SCORES=TABLE;
	TABLES K6SUM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCAPE42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDOWN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNRGY42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSOCA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADMALS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPALS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPAIN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADMWLM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPWLM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADOVER42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSMOK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPD42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPY42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINSA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADGENH42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINSB42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCLIM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDAYA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADLANG42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRISK42 / MISSPRINT  SCORES=TABLE;
	TABLES SFFLAG42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPRX42 / MISSPRINT  SCORES=TABLE;
	TABLES MCS42 / MISSPRINT  SCORES=TABLE;
	TABLES PCS42 / MISSPRINT  SCORES=TABLE;
	TABLES SEX / MISSPRINT  SCORES=TABLE;
	TABLES STRKDX / MISSPRINT  SCORES=TABLE;
	TABLES ERDEXP12 / MISSPRINT  SCORES=TABLE;
	TABLES OBDEXP12 / MISSPRINT  SCORES=TABLE;
	TABLES OBNEXP12 / MISSPRINT  SCORES=TABLE;
	TABLES OPSEXP12 / MISSPRINT  SCORES=TABLE;
	TABLES AMNEXP12 / MISSPRINT  SCORES=TABLE;
	TABLES EDUYRDEG / MISSPRINT  SCORES=TABLE;
	TABLES AMNURS12 / MISSPRINT  SCORES=TABLE;
	TABLES AMDRC12 / MISSPRINT  SCORES=TABLE;
	TABLES AMTOTC12 / MISSPRINT  SCORES=TABLE;
	TABLES ERTOT12 / MISSPRINT  SCORES=TABLE;
	TABLES IPDIS12 / MISSPRINT  SCORES=TABLE;
	TABLES OBASST12 / MISSPRINT  SCORES=TABLE;
	TABLES OBDRV12 / MISSPRINT  SCORES=TABLE;
	TABLES OPDRV12 / MISSPRINT  SCORES=TABLE;
	TABLES AMASST12 / MISSPRINT  SCORES=TABLE;
	TABLES AGE12X / MISSPRINT  SCORES=TABLE;
	TABLES ANGIDX / MISSPRINT  SCORES=TABLE;
	TABLES ARTHDX / MISSPRINT  SCORES=TABLE;
	TABLES ASTHDX / MISSPRINT  SCORES=TABLE;
	TABLES CANCERDX / MISSPRINT  SCORES=TABLE;
	TABLES REGION12 / MISSPRINT  SCORES=TABLE;
	TABLES CHDDX / MISSPRINT  SCORES=TABLE;
	TABLES AFDC12 / MISSPRINT  SCORES=TABLE;
	TABLES EICRDT12 / MISSPRINT  SCORES=TABLE;
	TABLES SAQELIG / MISSPRINT  SCORES=TABLE;
	TABLES EMPST31 / MISSPRINT  SCORES=TABLE;
	TABLES EMPST42 / MISSPRINT  SCORES=TABLE;
	TABLES EMPST53 / MISSPRINT  SCORES=TABLE;
	TABLES MCDEV12 / MISSPRINT  SCORES=TABLE;
	TABLES FAMINC12 / MISSPRINT  SCORES=TABLE;
	TABLES SAQWT12F / MISSPRINT  SCORES=TABLE;
	TABLES INSURC12 / MISSPRINT  SCORES=TABLE;
	TABLES MIDX / MISSPRINT  SCORES=TABLE;
	TABLES HIBPDX / MISSPRINT  SCORES=TABLE;
	TABLES INTVLANG / MISSPRINT  SCORES=TABLE;
	TABLES MARRY12X / MISSPRINT  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: RecodeVarriables   */
%LET _CLIENTTASKLABEL='RecodeVarriables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\nwarner\Assignments\WarnerN_SAS_project_01_09_15.egp';
%LET _CLIENTPROJECTNAME='WarnerN_SAS_project_01_09_15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_NATVARRIABLES_MANAGED);

PROC SQL;
   CREATE TABLE WORK."QUERY_FOR_NATVARRIABLES_MANAGED"n AS 
   SELECT t1.AMNURS12, 
          t1.AMDRC12, 
          t1.AMTOTC12, 
          t1.ERTOT12, 
          t1.IPDIS12, 
          t1.OBASST12, 
          t1.OBDRV12, 
          t1.OPDRV12, 
          t1.AMASST12, 
          t1.AGE12X, 
          t1.ANGIDX, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.CANCERDX, 
          t1.REGION12, 
          t1.CHDDX, 
          t1.AFDC12, 
          t1.EICRDT12, 
          t1.SAQELIG, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.MCDEV12, 
          t1.FAMINC12, 
          t1.SAQWT12F, 
          t1.INSURC12, 
          t1.MIDX, 
          t1.HIBPDX, 
          t1.INTVLANG, 
          t1.MARRY12X, 
          t1.DUPERSID, 
          t1.DIVDP12X, 
          t1.SSIP12X, 
          t1.TTLP12X, 
          t1.RACETHX, 
          t1.ADAPPT42, 
          t1.ADEXPL42, 
          t1.ADLIST42, 
          t1.ADTLHW42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADILWW42, 
          t1.ADRTWW42, 
          t1.ADFFRM42, 
          t1.ADRTCR42, 
          t1.ADSPEC42, 
          t1.ADFHLP42, 
          t1.ADHECR42, 
          t1.ADNSMK42, 
          t1.ADEGMC42, 
          t1.ADSPRF42, 
          t1.ADILCR42, 
          t1.ADNDCR42, 
          t1.ADDPRS42, 
          t1.ADINTR42, 
          t1.PHQ242, 
          t1.ADDRBP42, 
          t1.ADHOPE42, 
          t1.ADNERV42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADWRTH42, 
          t1.ADEFRT42, 
          t1.K6SUM42, 
          t1.ADCAPE42, 
          t1.ADDOWN42, 
          t1.ADNRGY42, 
          t1.ADSOCA42, 
          t1.ADMALS42, 
          t1.ADPALS42, 
          t1.ADPAIN42, 
          t1.ADMWLM42, 
          t1.ADPWLM42, 
          t1.ADOVER42, 
          t1.ADSMOK42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADINSA42, 
          t1.ADGENH42, 
          t1.ADINSB42, 
          t1.ADCLIM42, 
          t1.ADDAYA42, 
          t1.ADLANG42, 
          t1.ADRISK42, 
          t1.SFFLAG42, 
          t1.ADPRX42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.SEX, 
          t1.STRKDX, 
          t1.ERDEXP12, 
          t1.OBDEXP12, 
          t1.OBNEXP12, 
          t1.OPSEXP12, 
          t1.AMNEXP12, 
          t1.EDUYRDEG, 
          /* DOWN/DEPR */
            (CASE 
               WHEN -1 = t1.ADDOWN42 THEN .
               WHEN -8 = t1.ADDOWN42 THEN .
               WHEN -9 = t1.ADDOWN42 THEN .
               ELSE t1.ADDOWN42
            END) LABEL="ADDOWN42 SAQ: down/depr last 4wks (recoded minus missing resp)" AS 'DOWN/DEPR'n, 
          /* CALM/PEACEFUL */
            (CASE 
               WHEN -1 = t1.ADCAPE42 THEN .
               WHEN -8 = t1.ADCAPE42 THEN .
               WHEN -9 = t1.ADCAPE42 THEN .
               ELSE t1.ADCAPE42
            END) LABEL="ADCAPE42 SAQ 4wks: calm/peace (recoded minus missing resp)" AS 'CALM/PEACEFUL'n, 
          /* ENERGY */
            (CASE 
               WHEN -1 = t1.ADNRGY42 THEN .
               WHEN -9 = t1.ADNRGY42 THEN .
               ELSE t1.ADNRGY42
            END) LABEL="ADNRGY42 SAQ 4wks lots of energy (recoded minus missing resp)" AS ENERGY, 
          /* HEALTH PROHIBITED SOC */
            (CASE 
               WHEN -1 = t1.ADSOCA42 THEN .
               WHEN -9 = t1.ADSOCA42 THEN .
               ELSE t1.ADSOCA42
            END) LABEL="ADSOCA42 SAQ 4wks health stopped social activities (recoded minus missing resp)" AS 
            'HEALTH PROHIBITED SOC'n, 
          /* PAIN LIMITS WORK */
            (CASE 
               WHEN -1 = t1.ADPAIN42 THEN .
               WHEN -9 = t1.ADPAIN42 THEN .
               ELSE t1.ADPAIN42
            END) LABEL="ADPAIN42 SAQ 4wks pain limits normal work (recoded minus missing resp)" AS 'PAIN LIMITS WORK'n, 
          /* MNT PROBS WORK LIMIT */
            (CASE 
               WHEN -1 = t1.ADMWLM42 THEN .
               WHEN -7 = t1.ADMWLM42 THEN .
               WHEN -8 = t1.ADMWLM42 THEN .
               WHEN -9 = t1.ADMWLM42 THEN .
               ELSE t1.ADMWLM42
            END) LABEL="ADMWLM42 mental problems limit work (recoded minus missing resp)" AS 'MNT PROBS WORK LIMIT'n, 
          /* HLTH LIMITS MOD ACT */
            (CASE 
               WHEN -1 = t1.ADDAYA42 THEN .
               WHEN -9 = t1.ADDAYA42 THEN .
               ELSE t1.ADDAYA42
            END) LABEL="ADDAYA42 SAQ: health limits moderate activities (recoded minus missing resp)" AS 
            'HLTH LIMITS MOD ACT'n, 
          /* HLTH LIMITS STAIRS */
            (CASE 
               WHEN -1 = t1.ADCLIM42 THEN .
               WHEN -8 = t1.ADCLIM42 THEN .
               WHEN -9 = t1.ADCLIM42 THEN .
               ELSE t1.ADCLIM42
            END) LABEL="ADCLIM42 SAQ: health limits ability to climb stairs (recoded minus missing resp)" AS 
            'HLTH LIMITS STAIRS'n, 
          /* GENERAL HLTH */
            (CASE 
               WHEN -1 = t1.ADGENH42 THEN .
               WHEN -8 = t1.ADGENH42 THEN .
               WHEN -9 = t1.ADGENH42 THEN .
               ELSE t1.ADGENH42
            END) LABEL="ADGENH42 SAQ: health in general (ex-poor) (recoded minus missing resp)" AS 'GENERAL HLTH'n, 
          /* ACCMP LESS BC PHY PROBS */
            (CASE 
               WHEN -1 = t1.ADPALS42 THEN .
               WHEN -9 = t1.ADPALS42 THEN .
               ELSE t1.ADPALS42
            END) LABEL="ADPALS42 SAQ: 4wks acomplished less because of physical problems (recoded minus missing resp)" 
            AS 'ACCMP LESS BC PHY PROBS'n, 
          /* ACOMP LESS BS MNT PROBS */
            (CASE 
               WHEN -1 = t1.ADMALS42 THEN .
               WHEN -9 = t1.ADMALS42 THEN .
               ELSE t1.ADMALS42
            END) LABEL="ADMALS42 SAQ: 4wks acomplished less because of mental problems (recoded minus missing resp)" AS 
            'ACOMP LESS BS MNT PROBS'n, 
          /* PHY PROBS LIMIT WORK */
            (CASE 
               WHEN -1 = t1.ADPWLM42 THEN .
               WHEN -9 = t1.ADPWLM42 THEN .
               ELSE t1.ADPWLM42
            END) LABEL=
            "ADPWLM42 SAQ: 4wks physical problems limit work (oh nooooooo!!!!!!!!!!) (recoded minus missing resp)" AS 
            'PHY PROBS LIMIT WORK'n, 
          /* MARITAL */
            (CASE 
               WHEN -7 = t1.MARRY12X THEN .
               WHEN -9 = t1.MARRY12X THEN .
            END) LABEL="MARRY12X marital status (recoded minus missing resp)" AS MARITAL, 
          /* EDU */
            (CASE 
               WHEN -1 = t1.EDUYRDEG THEN .
               WHEN -7 = t1.EDUYRDEG THEN .
               WHEN -8 = t1.EDUYRDEG THEN .
               WHEN -9 = t1.EDUYRDEG THEN .
               ELSE t1.EDUYRDEG
            END) LABEL="EDUYRDEG education years and degrees (recoded minus missing resp)" AS EDU, 
          /* EMPLY31 */
            (CASE 
               WHEN -1 = t1.EMPST31 THEN .
               WHEN -7 = t1.EMPST31 THEN .
               WHEN -8 = t1.EMPST31 THEN .
               WHEN -9 = t1.EMPST31 THEN .
               ELSE t1.EMPST31
            END) LABEL="EMPST31 employment status on 3/1/12 (recoded minus missing resp)" AS EMPLY31, 
          /* EMPLY42 */
            (CASE 
               WHEN -1 = t1.EMPST42 THEN .
               WHEN -7 = t1.EMPST42 THEN .
               WHEN -8 = t1.EMPST42 THEN .
               WHEN -9 = t1.EMPST42 THEN .
               ELSE t1.EMPST42
            END) LABEL="EMPST42 employment status on 4/2/12 (recoded minus missing resp)" AS EMPLY42, 
          /* EMPLY53 */
            (CASE 
               WHEN -7 = t1.EMPST53 THEN .
               WHEN -8 = t1.EMPST53 THEN .
               WHEN -9 = t1.EMPST53 THEN .
               ELSE t1.EMPST53
            END) LABEL="EMPST53 employment status on 5/3/12" AS EMPLY53, 
          /* CANCER */
            (CASE 
               WHEN -7 = t1.CANCERDX THEN .
               WHEN -8 = t1.CANCERDX THEN .
               WHEN -9 = t1.CANCERDX THEN .
               ELSE t1.CANCERDX
            END) LABEL="CANCERDX any cancer diagnosis (recoded minus missing resp)" AS CANCER, 
          /* ASTHMA */
            (CASE 
               WHEN -7 = t1.ASTHDX THEN .
               WHEN -8 = t1.ASTHDX THEN .
               WHEN -9 = t1.ASTHDX THEN .
               ELSE t1.ASTHDX
            END) LABEL="ASTHDX asthma diagnosis (recoded minus missing resp)" AS ASTHMA, 
          /* ARTHRITIS */
            (CASE 
               WHEN -7 = t1.ARTHDX THEN .
               WHEN -8 = t1.ARTHDX THEN .
               WHEN -9 = t1.ARTHDX THEN .
               ELSE t1.ARTHDX
            END) LABEL="ARTHDX arthritis diagnosis (recoded minus missing resp)" AS ARTHRITIS, 
          /* HIBLOODPRESS */
            (CASE 
               WHEN -7 = t1.HIBPDX THEN .
               WHEN -8 = t1.HIBPDX THEN .
               WHEN -9 = t1.HIBPDX THEN .
               ELSE t1.HIBPDX
            END) LABEL="HIBPDX high blood pressure diagnosis (recoded minus missing resp)" AS HIBLOODPRESS, 
          /* STROKE */
            (CASE 
               WHEN -7 = t1.STRKDX THEN .
               WHEN -8 = t1.STRKDX THEN .
               WHEN -9 = t1.STRKDX THEN .
               ELSE t1.STRKDX
            END) LABEL="STRKDX stroke diagnosis (recoded minus missing resp)" AS STROKE, 
          /* HEARTATTK */
            (CASE 
               WHEN -7 = t1.MIDX THEN .
               WHEN -8 = t1.MIDX THEN .
               WHEN -9 = t1.MIDX THEN .
               ELSE t1.MIDX
            END) LABEL="MIDX heart attack diagnosis (recoded minus missing resp)" AS HEARTATTK, 
          /* ANGINA */
            (CASE 
               WHEN -7 = t1.ANGIDX THEN .
               WHEN -8 = t1.ANGIDX THEN .
               WHEN -9 = t1.ANGIDX THEN .
               ELSE t1.ANGIDX
            END) LABEL="ANGIDX angina diagnosis (recoded minus missing resp)" AS ANGINA, 
          /* EITC */
            (CASE 
               WHEN -1 = t1.EICRDT12 THEN .
               WHEN -7 = t1.EICRDT12 THEN .
               WHEN -8 = t1.EICRDT12 THEN .
               WHEN -9 = t1.EICRDT12 THEN .
               ELSE t1.EICRDT12
            END) LABEL="EICRDT12 did/will recieve earned income tax credit (recoded minus missing resp)" AS EITC, 
          /* MEDVISITSFORCARE */
            (CASE 
               WHEN -1 = t1.ADAPPT42 THEN .
               WHEN -8 = t1.ADAPPT42 THEN .
               WHEN -9 = t1.ADAPPT42 THEN .
               ELSE t1.ADAPPT42
            END) LABEL="ADAPPT42 SAQ: 12mo # visits to medical office for care (recoded minus missing resp)" AS 
            MEDVISITSFORCARE, 
          /* EASEGETTINGCARE */
            (CASE 
               WHEN -1 = t1.ADEGMC42 THEN .
               WHEN -9 = t1.ADEGMC42 THEN .
               ELSE t1.ADEGMC42
            END) LABEL="ADEGMC42 SAQ: 12mo ease getting needed medical care (recoded minus missing resp)" AS 
            EASEGETTINGCARE, 
          /* HCARERATING */
            (CASE 
               WHEN -1 = t1.ADHECR42 THEN .
               WHEN -9 = t1.ADHECR42 THEN .
               ELSE t1.ADHECR42
            END) LABEL="ADHECR42 SAQ: 12mo rating of health care (recoded minus missing resp)" AS HCARERATING, 
          /* DRLISTENED */
            (CASE 
               WHEN -1 = t1.ADLIST42 THEN .
               WHEN -7 = t1.ADLIST42 THEN .
               WHEN -9 = t1.ADLIST42 THEN .
               ELSE t1.ADLIST42
            END) LABEL="ADLIST42 SAQ: 12mo doctor listened to you (recoded minus missing resp)" AS DRLISTENED, 
          /* CARENEEDED */
            (CASE 
               WHEN -1 = t1.ADNDCR42 THEN .
               WHEN -8 = t1.ADNDCR42 THEN .
               WHEN -9 = t1.ADNDCR42 THEN .
               ELSE t1.ADNDCR42
            END) LABEL="ADNDCR42 SAQ: 12mo needed any care, treatment, test (recoded minus missing resp)" AS CARENEEDED, 
          /* FREQALLEFFORT */
            (CASE 
               WHEN -1 = t1.ADEFRT42 THEN .
               WHEN -7 = t1.ADEFRT42 THEN .
               WHEN -8 = t1.ADEFRT42 THEN .
               WHEN -9 = t1.ADEFRT42 THEN .
               ELSE t1.ADEFRT42
            END) LABEL="ADEFRT42 SAQ: 30 days how often everything an effort" AS FREQALLEFFORT, 
          /* FEELINGS */
            (CASE 
               WHEN -1 = t1.K6SUM42 THEN .
               WHEN -9 = t1.K6SUM42 THEN .
               ELSE t1.K6SUM42
            END) LABEL="K6SUM42 SAQ: 30 days overall rating of feelings (recoded minus missing resp)" AS FEELINGS, 
          /* MENTALSUM */
            (CASE 
               WHEN -1 = t1.MCS42 THEN .
               WHEN -9 = t1.MCS42 THEN .
               ELSE t1.MCS42
            END) LABEL="MCS42 SAQ: mental component summary SF-12v2 (recoded minus missing resp)" AS MENTALSUM, 
          /* PHYSUM */
            (CASE 
               WHEN -1 = t1.PCS42 THEN .
               WHEN -9 = t1.PCS42 THEN .
               ELSE t1.PCS42
            END) LABEL="PCS42 SAQ: physical component summary SF-12v2 (recoded minus missing resp)" AS PHYSUM
      FROM NATDATA.'NATVARRIABLESADULTS01.07.15'n t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies   */
%LET _CLIENTTASKLABEL='One-Way Frequencies';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\nwarner\Assignments\WarnerN_SAS_project_01_09_15.egp';
%LET _CLIENTPROJECTNAME='WarnerN_SAS_project_01_09_15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 08, 2015 at 1:52:14 PM
   By task: One-Way Frequencies

   Input Data: Local:WORK.QUERY_FOR_NATVARRIABLES_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_NATVARRIABLES_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.AMNURS12, T.AMDRC12, T.AMTOTC12, T.ERTOT12, T.IPDIS12, T.OBASST12, T.OBDRV12, T.OPDRV12, T.AMASST12, T.AGE12X, T.ANGIDX, T.ARTHDX, T.ASTHDX, T.CANCERDX, T.REGION12, T.CHDDX, T.AFDC12, T.EICRDT12, T.SAQELIG, T.EMPST31, T.EMPST42
		     , T.EMPST53, T.MCDEV12, T.FAMINC12, T.SAQWT12F, T.INSURC12, T.MIDX, T.HIBPDX, T.INTVLANG, T.MARRY12X, T.DIVDP12X, T.SSIP12X, T.TTLP12X, T.RACETHX, T.ADAPPT42, T.ADEXPL42, T.ADLIST42, T.ADTLHW42, T.ADINST42, T.ADEZUN42
		     , T.ADRESP42, T.ADPRTM42, T.ADILWW42, T.ADRTWW42, T.ADFFRM42, T.ADRTCR42, T.ADSPEC42, T.ADFHLP42, T.ADHECR42, T.ADNSMK42, T.ADEGMC42, T.ADSPRF42, T.ADILCR42, T.ADNDCR42, T.ADDPRS42, T.ADINTR42, T.PHQ242, T.ADDRBP42, T.ADHOPE42
		     , T.ADNERV42, T.ADREST42, T.ADSAD42, T.ADWRTH42, T.ADEFRT42, T.K6SUM42, T.ADCAPE42, T.ADDOWN42, T.ADNRGY42, T.ADSOCA42, T.ADMALS42, T.ADPALS42, T.ADPAIN42, T.ADMWLM42, T.ADPWLM42, T.ADOVER42, T.ADSMOK42, T.ADCMPD42, T.ADCMPM42
		     , T.ADCMPY42, T.ADINSA42, T.ADGENH42, T.ADINSB42, T.ADCLIM42, T.ADDAYA42, T.ADLANG42, T.ADRISK42, T.SFFLAG42, T.ADPRX42, T.MCS42, T.PCS42, T.SEX, T.STRKDX, T.ERDEXP12, T.OBDEXP12, T.OBNEXP12, T.OPSEXP12, T.AMNEXP12
		     , T.EDUYRDEG, T."DOWN/DEPR"n, T."CALM/PEACEFUL"n, T.ENERGY, T."HEALTH PROHIBITED SOC"n, T."PAIN LIMITS WORK"n, T."MNT PROBS WORK LIMIT"n, T."HLTH LIMITS MOD ACT"n, T."HLTH LIMITS STAIRS"n, T."GENERAL HLTH"n
		     , T."ACCMP LESS BC PHY PROBS"n, T."ACOMP LESS BS MNT PROBS"n, T."PHY PROBS LIMIT WORK"n, T.MARITAL, T.EDU, T.EMPLY31, T.EMPLY42, T.EMPLY53, T.CANCER, T.ASTHMA, T.ARTHRITIS, T.HIBLOODPRESS, T.STROKE, T.HEARTATTK, T.ANGINA, T.EITC, T.MEDVISITSFORCARE
		     , T.EASEGETTINGCARE, T.HCARERATING, T.DRLISTENED, T.CARENEEDED, T.FREQALLEFFORT, T.FEELINGS, T.MENTALSUM, T.PHYSUM
	FROM WORK.QUERY_FOR_NATVARRIABLES_MANAGED as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results post-recoding; wanted to make some graphs (plots) for kicks and giggles";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Nat Warner";
ODS GRAPHICS ON;
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES AMNURS12 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES AMDRC12 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES AMTOTC12 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ERTOT12 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES IPDIS12 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES OBASST12 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES OBDRV12 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES OPDRV12 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES AMASST12 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES AGE12X / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ANGIDX / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ARTHDX / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ASTHDX / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES CANCERDX / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES REGION12 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES CHDDX / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES AFDC12 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES EICRDT12 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES SAQELIG / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES EMPST31 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES EMPST42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES EMPST53 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES MCDEV12 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES FAMINC12 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES SAQWT12F / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES INSURC12 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES MIDX / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES HIBPDX / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES INTVLANG / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES MARRY12X / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES DIVDP12X / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES SSIP12X / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES TTLP12X / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES RACETHX / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADAPPT42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADEXPL42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADLIST42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADTLHW42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADINST42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADEZUN42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADRESP42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADPRTM42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADILWW42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADRTWW42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADFFRM42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADRTCR42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADSPEC42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADFHLP42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADHECR42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADNSMK42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADEGMC42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADSPRF42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADILCR42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADNDCR42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADDPRS42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADINTR42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES PHQ242 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADDRBP42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADHOPE42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADNERV42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADREST42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADSAD42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADWRTH42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADEFRT42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES K6SUM42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADCAPE42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADDOWN42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADNRGY42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADSOCA42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADMALS42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADPALS42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADPAIN42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADMWLM42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADPWLM42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADOVER42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADSMOK42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADCMPD42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADCMPM42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADCMPY42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADINSA42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADGENH42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADINSB42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADCLIM42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADDAYA42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADLANG42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADRISK42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES SFFLAG42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ADPRX42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES MCS42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES PCS42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES SEX / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES STRKDX / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ERDEXP12 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES OBDEXP12 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES OBNEXP12 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES OPSEXP12 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES AMNEXP12 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES EDUYRDEG / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES "DOWN/DEPR"n / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES "CALM/PEACEFUL"n / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ENERGY / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES "HEALTH PROHIBITED SOC"n / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES "PAIN LIMITS WORK"n / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES "MNT PROBS WORK LIMIT"n / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES "HLTH LIMITS MOD ACT"n / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES "HLTH LIMITS STAIRS"n / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES "GENERAL HLTH"n / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES "ACCMP LESS BC PHY PROBS"n / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES "ACOMP LESS BS MNT PROBS"n / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES "PHY PROBS LIMIT WORK"n / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES MARITAL / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES EDU / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES EMPLY31 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES EMPLY42 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES EMPLY53 / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES CANCER / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ASTHMA / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ARTHRITIS / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES HIBLOODPRESS / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES STROKE / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES HEARTATTK / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES ANGINA / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES EITC / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES MEDVISITSFORCARE / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES EASEGETTINGCARE / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES HCARERATING / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES DRLISTENED / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES CARENEEDED / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES FREQALLEFFORT / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES FEELINGS / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES MENTALSUM / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
	TABLES PHYSUM / MISSPRINT  SCORES=TABLE plots(only orient=horizontal)=freq;
RUN;
ODS GRAPHICS OFF;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: OrigionalData01.07.15   */
%LET _CLIENTTASKLABEL='OrigionalData01.07.15';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\nwarner\Assignments\WarnerN_SAS_project_01_09_15.egp';
%LET _CLIENTPROJECTNAME='WarnerN_SAS_project_01_09_15.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\Data\MEPS";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 08, 2015 at 1:52:16 PM
   By task: OrigionalData01.07.15

   Input Data: P:\QAC\qac200\Data\MEPS\meps_fullyr_2012.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsFormeps_fullyr_2012);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=ECLIB000.meps_fullyr_2012 OUT=WORK.SUCOUT1;

RUN;

DATA WORK.CONTContentsFormeps_fullyr_2012(LABEL="Contents Details for meps_fullyr_2012");
   SET WORK.SUCOUT1;
RUN;

PROC DELETE DATA=WORK.SUCOUT1;
RUN;

%LET _LINESIZE=%SYSFUNC(GETOPTION(LINESIZE));

PROC SQL;
CREATE VIEW WORK.SCVIEW AS 
	SELECT DISTINCT memname LABEL="Table Name", 
			memlabel LABEL="Label", 
			memtype LABEL="Type", 
			crdate LABEL="Date Created", 
			modate LABEL="Date Modified", 
			nobs LABEL="Number of Obs.", 
			charset LABEL="Char. Set", 
			protect LABEL="Password Protected", 
			typemem LABEL="Data Set Type" FROM WORK.CONTContentsFormeps_fullyr_2012
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='MEPS_FULLYR_2012';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=WORK.CONTContentsFormeps_fullyr_2012 OUT=WORK.CONTContentsFormeps_fullyr_2012;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.CONTContentsFormeps_fullyr_2012
		WHERE memname='MEPS_FULLYR_2012';
QUIT;

PROC REPORT DATA=WORK.SCTABLE NOWINDOWS; 
   FORMAT TYPE _EG_VARTYPE.; 
   DEFINE LABEL / DISPLAY WIDTH=&_LINESIZE; 
   LABEL NAME="Name" LABEL="Label" TYPE="Type" LENGTH="Length" INFORMAT="Informat" FORMAT="Format"; 
   BY memname NOTSORTED;  
   COLUMN name varnum type format label length;  
 QUIT;  

PROC SQL;
	DROP TABLE WORK.SCTABLE;
	DROP VIEW WORK.SCVIEW;
QUIT;

PROC CATALOG CATALOG=WORK.FORMATS;
   DELETE _EG_VARTYPE / ENTRYTYPE=FORMAT;
RUN;
OPTIONS BYLINE;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
