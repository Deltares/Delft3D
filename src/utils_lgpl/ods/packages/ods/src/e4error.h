/* e4error.h  (c)Copyright Sequiter Software Inc., 1990-2024.  All rights reserved. */
/*            (c)Copyright Sequiter Software Inc.,1990-2024. Alle Rechte vorbehalten.  */
/*                German translation: Mgr. Gertruda TKACIKOVA, Jan. 1992.  */

/*   error messages  */

/* function names - language independent */
#ifdef S4OFF_ERROR
   #define E4_BM4DESTROY     0
   #define E4_BM4COLLAPSE    0
   #define E4_BM4COMBINE     0
   #define E4_BM4COMBINE_LF  0
   #define E4_BM4CON_COMBINE 0
   #define E4_BM4COPY        0
   #define E4_BM4FLAG_GEN    0
   #define E4_BM4REDIST      0
   #define E4_BM4SEEK        0
   #define E4_B4ALLOC        0
   #define E4_B4APPEND       0
   #define E4_B4FREE         0
   #define E4_B4GET_LK       0
   #define E4_B4INSERT       0
   #define E4_B4INSERT_BR    0
   #define E4_B4KEY          0
   #define E4_B4LEAF_SEEK    0
   #define E4_B4REMOVE       0
   #define E4_CONST4EQ       0
   #define E4_CONST4LESS_EQ  0
   #define E4_CONST4LESS     0
   #define E4_C4BCD_FROM_A   0
   #define E4_C4DTOA45       0
   #define E4_DATA_LIST4REM  0
   #define E4_DATE4ASSIGN    0
   #define E4_D4ALIAS        0
   #define E4_D4ALIAS_SET    0
   #define E4_D4APPEND       0
   #define E4_D4APPEND_BL    0
   #define E4_D4APPEND_DATA  0
   #ifdef S4VBASIC
      #define E4_D4APPEND_STRT  0
   #else
      #define E4_D4APPEND_STRT  0
   #endif
   #define E4_D4BLANK        0
   #define E4_D4BOF          0
   #define E4_D4BOTTOM       0
   #define E4_D4CHANGED      0
   #define E4_D4CHECK        0
   #define E4_D4CLOSE        0
   #ifdef S4VBASIC
      #define E4_D4CLOSE_ALL    0
   #else
      #define E4_D4CLOSE_ALL    0
   #endif
   #define E4_D4CREATE       0
   #define E4_D4DATA         0
   #define E4_D4DELETE       0
   #define E4_D4DELETED      0
   #define E4_D4EOF          0
   #define E4_D4FIELD        0
   #define E4_D4FIELD_INFO   0
   #define E4_D4FIELDJ       0
   #define E4_D4FIELD_NUM    0
   #define E4_D4FLUSH        0
   #define E4_D4FLUSH_DATA   0
   #define E4_D4FLUSH_FILES  0
   #define E4_D4FREE_BLOCKS  0
   #define E4_D4GO           0
   #define E4_D4GO_DATA      0
   #define E4_D4GO_EOF       0
   #define E4_D4INDEX        0
   #define E4_D4INIT         0
   #define E4_D4INIT_UNDO    0
   #define E4_D4LOCK         0
   #define E4_D4LOCK_ALL     0
   #ifdef S4VBASIC
      #define E4_D4LOCK_APP     0
      #define E4_D4LOCK_FILE    0
      #define E4_D4LOCK_INDEX   0
   #else
      #define E4_D4LOCK_APP     0
      #define E4_D4LOCK_FILE    0
      #define E4_D4LOCK_INDEX   0
   #endif
   #define E4_D4LOCK_GROUP      0
   #define E4_D4REINDEX         0
   #define E4_D4LOCK_TEST       0
   #define E4_D4LOCK_TEST_APP   0
   #define E4_D4LOCK_TEST_FL    0
   #define E4_D4LOCK_TEST_IN    0
   #ifdef S4VBASIC
      #define E4_D4MEMO_COMP      0
   #else
      #define E4_D4MEMO_COMP      0
   #endif
   #define E4_D4NUM_FIELDS   0
   #define E4_D4PACK         0
   #define E4_D4PACK_DATA    0
   #define E4_D4OPEN         0
   #define E4_D4OPT_START    0
   #define E4_D4OPT_SUSPEND  0
   #define E4_D4OPTIMIZE     0
   #define E4_D4OPT_WRITE    0
   #define E4_D4POS          0
   #define E4_D4POS_SET      0
   #define E4_D4READ         0
   #define E4_D4READ_OLD     0
   #define E4_D4RECALL       0
   #define E4_D4RECCOUNT     0
   #define E4_D4RECNO        0
   #define E4_D4RECORD       0
   #define E4_D4RECORD_POS   0
   #define E4_D4RECORD_WIDTH 0
   #define E4_D4REFRESH      0
   #define E4_D4REFRESH_REC  0
   #define E4_D4REINDEX      0
   #define E4_D4SEEK         0
   #ifdef S4VBASIC
      #define E4_D4SEEK_DBL     0
   #else
      #define E4_D4SEEK_DBL     0
   #endif
   #define E4_D4SKIP         0
   #define E4_D4TAG          0
   #ifdef S4VBASIC
      #define E4_D4TAG_DEF      0
   #else
      #define E4_D4TAG_DEF      0
   #endif
   #ifdef S4VBASIC
      #define E4_D4TAG_NEXT     0
   #else
      #define E4_D4TAG_NEXT     0
   #endif
   #ifdef S4VBASIC
      #define E4_D4TAG_PREV     0
   #else
      #define E4_D4TAG_PREV     0
   #endif
   #ifdef S4VBASIC
      #define E4_D4TAG_SELECT   0
   #else
      #define E4_D4TAG_SELECT   0
   #endif
   #define E4_D4TAG_SELECTED 0
   #define E4_D4TOP          0
   #define E4_D4UNLOCK       0
   #define E4_D4UNLOCK_AP    0
   #define E4_D4UNLOCK_DATA  0
   #ifdef S4VBASIC
      #define E4_D4UNLOCK_FILE  0
   #else
      #define E4_D4UNLOCK_FILE  0
   #endif
   #define E4_D4UNLOCK_FILES 0
   #define E4_D4UNLOCK_INDEX 0
   #define E4_D4UNLOCK_REC   0
   #define E4_D4UPDATE       0
   #define E4_D4UPDATE_HDR   0
   #define E4_D4UPDATE_PRIO  0
   #define E4_D4UPDATE_REC   0
   #define E4_D4VALID_MID    0
   #define E4_D4WRITE        0
   #define E4_D4WRITE_DATA   0
   #define E4_D4WRITE_KEYS   0
   #define E4_D4ZAP          0
   #define E4_D4ZAP_DATA     0
   #define E4_EXPR4DOUBLE    0
   #define E4_EXPR4EXEC      0
   #define E4_EXPR4KEY       0
   #define E4_EXPR4LOOKUP    0
   #define E4_EXPR4PARSE     0
   #define E4_EXPR4PV        0
   #define E4_EXPR4TRUE      0
   #define E4_EXPR4VARY      0
   #define E4_EXPR_OM        0
   #define E4_FILE_LEN       0
   #define E4_FILE4LEN       0
   #define E4_FILE4LEN_SET   0
   #define E4_F4ASSIGN       0
   #ifdef S4VBASIC
      #define E4_F4ASSIGN_CHAR  0
      #define E4_F4ASSIGN_DBL   0
      #define E4_F4ASSIGN_FLD   0
      #define E4_F4ASSIGN_INT   0
      #define E4_F4ASSIGN_LONG  0
   #else
      #define E4_F4ASSIGN_CHAR  0
      #define E4_F4ASSIGN_DBL   0
      #define E4_F4ASSIGN_FLD   0
      #define E4_F4ASSIGN_INT   0
      #define E4_F4ASSIGN_LONG  0
   #endif
   #define E4_F4ASSIGN_N     0
   #define E4_F4ASSIGN_PTR   0
   #define E4_F4BLANK        0
   #define E4_F4CHAR         0
   #define E4_F4CREATE       0
   #define E4_F4DATA         0
   #define E4_F4DECIMALS     0
   #define E4_F4DOUBLE       0
   #define E4_F4DOUBLE2      0
   #define E4_F4FLUSH        0
   #define E4_F4INT          0
   #define E4_F4LAG_AND      0
   #define E4_F4LAG_OR       0
   #define E4_F4LAG_IS_SET   0
   #define E4_F4LAG_RESET    0
   #define E4_F4LAG_SET      0
   #define E4_F4LEN          0
   #define E4_F4LOCK         0
   #define E4_F4LONG         0
   #define E4_F4LOW_FLUSH    0
   #define E4_F4MEMO_ASSIGN  0
   #ifdef S4VBASIC
      #define E4_F4MEMO_ASS_N   0
   #else
      #define E4_F4MEMO_ASS_N   0
   #endif
   #define E4_F4MEMO_CHECK   0
   #define E4_F4MEMO_FLUSH   0
   #define E4_F4MEMO_FREE    0
   #define E4_F4MEMO_LEN     0
   #define E4_F4MEMO_NCPY    0
   #define E4_F4MEMO_PTR     0
   #define E4_F4MEMO_READ    0
   #define E4_F4MEMO_READ_LW 0
   #define E4_F4MEMO_RESET   0
   #define E4_F4MEMO_SET_LEN 0
   #define E4_F4MEMO_STR     0
   #define E4_F4MEMO_UPDATE  0
   #define E4_F4MEMO_WRITE   0
   #define E4_F4NAME         0
   #define E4_F4NCY          0
   #define E4_F4OPEN         0
   #define E4_F4OPTIMIZE     0
   #define E4_F4OPTIMIZE_WR  0
   #define E4_F4PTR          0
   #define E4_F4READ         0
   #define E4_F4READ_ALL     0
   #define E4_F4REFRESH      0
   #define E4_F4REPLACE      0
   #define E4_F4STR          0
   #define E4_F4TEMP         0
   #define E4_F4SEQ_READ     0
   #define E4_F4SEQ_READ_ALL 0
   #define E4_F4SEQ_READ_IN  0
   #define E4_F4SEQ_WRITE    0
   #define E4_F4SEQ_WRT_FLSH 0
   #define E4_F4SEQ_WRT_INIT 0
   #define E4_F4SEQ_WRT_REP  0
   #define E4_F4TRUE         0
   #define E4_F4TYPE         0
   #define E4_F4UNLOCK       0
   #define E4_F4WRITE        0
   #define E4_I4ADD_TAG      0
   #define E4_I4CHECK        0
   #define E4_I4CLOSE        0
   #define E4_I4CREATE       0
   #define E4_I4EXTEND       0
   #define E4_I4FLUSH        0
   #define E4_I4GET_LAST_KEY 0
   #define E4_I4LOCK         0
   #define E4_I4OPEN         0
   #define E4_I4READ_BLOCK   0
   #define E4_I4REINDEX      0
   #define E4_I4REINDEX_ADD  0
   #define E4_I4REINDEX_BA   0
   #define E4_I4REINDEX_FN   0
   #define E4_I4REINDEX_SK   0
   #define E4_I4REINDEX_TD   0
   #define E4_I4REINDEX_THW  0
   #define E4_I4REINDEX_THWS 0
   #define E4_I4REINDEX_WK   0
   #define E4_I4SHRINK       0
   #define E4_I4TAG          0
   #define E4_I4TAG_INFO     0
   #define E4_I4UNLOCK       0
   #define E4_I4UPDATE       0
   #define E4_I4UPDATE_HDR   0
   #define E4_I4VERSION_CHK  0
   #define E4_LOG4TRUE       0
   #define E4_L4ADD          0
   #define E4_L4ADD_AFTER    0
   #define E4_L4ADD_BEFORE   0
   #define E4_L4CHECK        0
   #define E4_L4FIRST        0
   #define E4_L4LAST         0
   #define E4_L4NEXT         0
   #define E4_L4POP          0
   #define E4_L4PREV         0
   #define E4_L4REMOVE       0
   #define E4_MEMO4FILE_CR   0
   #define E4_MEMO4FILE_RD   0
   #define E4_MEMO4FILE_WR   0
   #define E4_MEM4ALLOC      0
   #define E4_MEM4CREATE     0
   #define E4_MEM4FREE       0
   #define E4_OPT4BLOCK_ADD  0
   #define E4_OPT4BLOCK_CLR  0
   #define E4_OPT4BLOCK_FLSH 0
   #define E4_OPT4BLOCK_REM  0
   #define E4_OPT4FILE_CH    0
   #define E4_OPT4FILE_DEL   0
   #define E4_OPT4FILE_FL    0
   #define E4_OPT4FILE_GB    0
   #define E4_OPT4FILE_LRUB  0
   #define E4_OPT4FILE_LRUT  0
   #define E4_OPT4FILE_RB    0
   #define E4_OPT4FILE_RD    0
   #define E4_OPT4FILE_RD_FL 0
   #define E4_OPT4FILE_RD_SB 0
   #define E4_OPT4FILE_WR    0
   #define E4_OPT4FLUSH_ALL  0
   #define E4_OPT4FLUSH_LST  0
   #define E4_OPT4FLUSH_WB   0
   #define E4_OPT4FREE       0
   #define E4_OPT4INIT       0
   #define E4_OPT4SET_PRIO   0
   #define E4_R4BOTTOM       0
   #define E4_R4CHANGED      0
   #define E4_R4CREATE_SLAVE 0
   #define E4_R4DO           0
   #define E4_R4DO_ONE       0
   #define E4_R4EOF          0
   #define E4_R4FREE         0
   #define E4_R4INIT         0
   #define E4_R4LOCK         0
   #define E4_R4LOOKUP       0
   #define E4_R4MATCH_LEN    0
   #define E4_R4NEXT         0
   #define E4_R4NEXT_RIS     0
   #define E4_R4PREV_RIS     0
   #define E4_R4QUERY_SET    0
   #define E4_R4READ_REST    0
   #define E4_R4SKIP         0
   #define E4_R4SORT         0
   #define E4_R4SORT_SET     0
   #define E4_R4TOP          0
   #define E4_RESULT_CMP     0
   #define E4_R4UNLOCK       0
   #define E4_S4GET          0
   #define E4_S4GET_INIT     0
   #define E4_S4GET_MEM_INIT 0
   #define E4_S4INIT         0
   #define E4_S4SPOOLS_INIT  0
   #define E4_S4NEXT_SP_ENT  0
   #define E4_S4PUT          0
   #define E4_T4ADD          0
   #define E4_T4ADD_CALC     0
   #define E4_T4BALANCE_BR   0
   #define E4_T4BLOCK        0
   #define E4_T4BLOCK_CHK    0
   #define E4_T4BOTTOM       0
   #define E4_T4CHECK        0
   #define E4_T4CLOSE        0
   #define E4_T4CREATE       0
   #define E4_T4DOWN         0
   #define E4_T4DO_VERSION   0
   #define E4_T4DUMP         0
   #define E4_T4EOF          0
   #define E4_T4EXTEND       0
   #define E4_T4FLUSH        0
   #define E4_T4FREE_ALL     0
   #define E4_T4GO           0
   #define E4_T4INIT         0
   #define E4_T4KEY          0
   #define E4_T4OPEN         0
   #define E4_T4POSITION     0
   #define E4_T4POSITION2    0
   #define E4_T4POSITION_SET 0
   #define E4_T4RECNO        0
   #define E4_T4REMOVE       0
   #define E4_T4REM_CALC     0
   #define E4_T4SEEK         0
   #define E4_T4SKIP         0
   #define E4_T4TOP          0
   #define E4_T4UP           0
   #define E4_T4UPDATE       0
   #define E4_T4UP_TO_ROOT   0
   #define E4_U4ALLOC_AGN    0
   #define E4_U4NAME_EXT     0
   #define E4_U4NAME_PIECE   0
   #define E4_U4NAME_RET_EXT 0
   #define E4_U4NCPY         0
#else
   #define E4_BM4DESTROY     "bitmap4destroy()"
   #define E4_BM4COLLAPSE    "bitmap4collapse()"
   #define E4_BM4COMBINE     "bitmap4combine..xx_xx()"
   #define E4_BM4COMBINE_LF  "bitmap4combine_leafs()"
   #define E4_BM4CON_COMBINE "bitmap4constant_combine()"
   #define E4_BM4COPY        "bitmap4copy()"
   #define E4_BM4FLAG_GEN    "bitmap4flag_generate()"
   #define E4_BM4REDIST      "bitmap4redistribute()"
   #define E4_BM4SEEK        "bitmap4seek()"
   #define E4_B4ALLOC        "b4alloc()"
   #define E4_B4APPEND       "b4append()"
   #define E4_B4FREE         "b4free()"
   #define E4_B4GET_LK       "b4get_last_key()"
   #define E4_B4INSERT       "b4insert()"
   #define E4_B4INSERT_BR    "b4insert_branch()"
   #define E4_B4KEY          "b4key()"
   #define E4_B4LEAF_SEEK    "b4bleaf_seek()"
   #define E4_B4REMOVE       "b4remove()"
   #define E4_CONST4EQ       "const4eq()"
   #define E4_CONST4LESS_EQ  "const4less_eq()"
   #define E4_CONST4LESS     "const4less()"
   #define E4_C4BCD_FROM_A   "c4bcd_from_a()"
   #define E4_C4DTOA45       "c4dtoa45()"
   #define E4_DATA_LIST4REM  "data_list4remove()"
   #define E4_DATE4ASSIGN    "date4assign()"
   #define E4_D4ALIAS        "d4alias()"
   #ifdef S4VBASIC
      #define E4_D4ALIAS_SET    "d4aliasSet()"
   #else
      #define E4_D4ALIAS_SET    "d4alias_set()"
   #endif
   #define E4_D4APPEND       "d4append()"
   #ifdef S4VBASIC
      #define E4_D4APPEND_BL    "d4appendBlank()"
   #else
      #define E4_D4APPEND_BL    "d4append_blank()"
   #endif
   #define E4_D4APPEND_DATA  "d4append_data()"
   #ifdef S4VBASIC
      #define E4_D4APPEND_STRT  "d4appendStart()"
   #else
      #define E4_D4APPEND_STRT  "d4append_start()"
   #endif
   #define E4_D4BLANK        "d4blank()"
   #define E4_D4BOF          "d4bof()"
   #define E4_D4BOTTOM       "d4bottom()"
   #define E4_D4CHANGED      "d4changed()"
   #define E4_D4CHECK        "d4check()"
   #define E4_D4CLOSE        "d4close()"
   #ifdef S4VBASIC
      #define E4_D4CLOSE_ALL    "d4closeAll()"
   #else
      #define E4_D4CLOSE_ALL    "d4close_all()"
   #endif
   #define E4_D4CREATE       "d4create()"
   #define E4_D4DATA         "d4data()"
   #define E4_D4DELETE       "d4delete()"
   #define E4_D4DELETED      "d4deleted()"
   #define E4_D4EOF          "d4eof()"
   #define E4_D4FIELD        "d4field()"
   #ifdef S4VBASIC
      #define E4_D4FIELD_INFO   "d4fieldInfo()"
      #define E4_D4FIELDJ       "d4fieldJ()"
      #define E4_D4FIELD_NUM    "d4fieldNumber()"
   #else      
      #define E4_D4FIELD_INFO   "d4field_info()"
      #define E4_D4FIELDJ       "d4fieldj()"
      #define E4_D4FIELD_NUM    "d4field_number()"
   #endif
   #define E4_D4FLUSH        "d4flush()"
   #ifdef S4VBASIC   
      #define E4_D4FLUSH_DATA   "d4flushData()"
      #define E4_D4FLUSH_FILES  "d4flushFiles()"
      #define E4_D4FREE_BLOCKS  "d4freeBlocks()"
   #else      
      #define E4_D4FLUSH_DATA   "d4flush_data()"
      #define E4_D4FLUSH_FILES  "d4flush_files()"
      #define E4_D4FREE_BLOCKS  "d4free_blocks()"
   #endif
   #define E4_D4GO           "d4go()"
   #ifdef S4VBASIC
      #define E4_D4GO_DATA      "d4goData()"
      #define E4_D4GO_EOF       "d4goEof()"
   #else
      #define E4_D4GO_DATA      "d4go_data()"
      #define E4_D4GO_EOF       "d4go_eof()"
   #endif
   #define E4_D4INDEX        "d4index()"
   #define E4_D4INIT         "d4init()"
   #ifdef S4VBASIC
      #define E4_D4INIT_UNDO    "d4initUndo()"
   #else
      #define E4_D4INIT_UNDO    "d4init_undo()"
   #endif
   #define E4_D4LOCK         "d4lock()"
   #ifdef S4VBASIC
      #define E4_D4LOCK_ALL     "d4lockAll()"
      #define E4_D4LOCK_APP     "d4lockAppend()"
      #define E4_D4LOCK_FILE    "d4lockFile()"
      #define E4_D4LOCK_INDEX   "d4lockIndex()"
      #define E4_D4LOCK_GROUP   "d4lockGroup()"
   #else
      #define E4_D4LOCK_ALL     "d4lock_all()"
      #define E4_D4LOCK_APP     "d4lock_append()"
      #define E4_D4LOCK_FILE    "d4lock_file()"
      #define E4_D4LOCK_INDEX   "d4lock_index():"
      #define E4_D4LOCK_GROUP   "d4lock_group()"
   #endif
   #define E4_D4REINDEX      "d4reindex()"
   #ifdef S4VBASIC
      #define E4_D4LOCK_TEST       "d4lockTest()"
      #define E4_D4LOCK_TEST_APP   "d4lockTestAppend()"
      #define E4_D4LOCK_TEST_FL    "d4lockTestFile()"
      #define E4_D4LOCK_TEST_IN    "d4lockTestIndex()"
      #define E4_D4MEMO_COMP       "d4memoCompress():"
      #define E4_D4NUM_FIELDS      "d4numFields()"
   #else
      #define E4_D4LOCK_TEST       "d4lock_test()"
      #define E4_D4LOCK_TEST_APP   "d4lock_test_append()"
      #define E4_D4LOCK_TEST_FL    "d4lock_test_file()"
      #define E4_D4LOCK_TEST_IN    "d4lock_test_index()"
      #define E4_D4MEMO_COMP       "d4memo_compress()"
      #define E4_D4NUM_FIELDS      "d4num_fields()"
   #endif
   #define E4_D4PACK         "d4pack()"
   #ifdef S4VBASIC
      #define E4_D4PACK_DATA    "d4packData()"
   #else
      #define E4_D4PACK_DATA    "d4pack_data()"
   #endif
   #define E4_D4OPEN         "d4open()"
   #ifdef S4VBASIC
      #define E4_D4OPT_START    "d4optStart()"
      #define E4_D4OPT_SUSPEND  "d4optSuspend()"
   #else
      #define E4_D4OPT_START    "d4opt_start()"
      #define E4_D4OPT_SUSPEND  "d4opt_suspend()"
   #endif
   #define E4_D4OPTIMIZE     "d4optimize()"
   #ifdef S4VBASIC
      #define E4_D4OPT_WRITE    "d4optimizeWrite()"
   #else
      #define E4_D4OPT_WRITE    "d4optimize_write()"
   #endif
   #define E4_D4POS          "d4position()"
   #ifdef S4VBASIC
      #define E4_D4POS_SET      "d4positionSet()"
   #else
      #define E4_D4POS_SET      "d4position_set()"
   #endif
   #define E4_D4READ         "d4read()"
   #define E4_D4READ_OLD     "d4read_old()"
   #define E4_D4RECALL       "d4recall()"
   #define E4_D4RECCOUNT     "d4reccount()"
   #define E4_D4RECNO        "d4recno()"
   #define E4_D4RECORD       "d4record()"
   #ifdef S4VBASIC
      #define E4_D4RECORD_POS   "d4recordPosition()"
      #define E4_D4RECORD_WIDTH "d4recordWidth()"
   #else
      #define E4_D4RECORD_POS   "d4record_position()"
      #define E4_D4RECORD_WIDTH "d4record_width()"
   #endif
   #define E4_D4REFRESH      "d4refresh()"
   #ifdef S4VBASIC
      #define E4_D4REFRESH_REC  "d4refreshRecord()"
   #else
      #define E4_D4REFRESH_REC  "d4refresh_record()"
   #endif
   #define E4_D4REINDEX      "d4reindex()"
   #define E4_D4SEEK         "d4seek()"
   #ifdef S4VBASIC
      #define E4_D4SEEK_DBL     "d4seekDouble()"
   #else
      #define E4_D4SEEK_DBL     "d4seek_double()"
   #endif
   #define E4_D4SKIP         "d4skip()"
   #define E4_D4TAG          "d4tag()"
   #ifdef S4VBASIC
      #define E4_D4TAG_DEF      "d4tagDefault()"
   #else
      #define E4_D4TAG_DEF      "d4tag_default()"
   #endif
   #ifdef S4VBASIC
      #define E4_D4TAG_NEXT     "d4tagNext()"
   #else
      #define E4_D4TAG_NEXT     "d4tag_next()"
   #endif
   #ifdef S4VBASIC
      #define E4_D4TAG_PREV     "d4tagPrev()"
   #else
      #define E4_D4TAG_PREV     "d4tag_prev()"
   #endif
   #ifdef S4VBASIC
      #define E4_D4TAG_SELECT   "d4tagSelect()"
      #define E4_D4TAG_SELECTED "d4tagSelected()"
   #else
      #define E4_D4TAG_SELECT   "d4tag_select()"
      #define E4_D4TAG_SELECTED "d4tag_selected()"
   #endif
   #define E4_D4TOP          "d4top()"
   #define E4_D4UNLOCK       "d4unlock()"
   #define E4_D4UNLOCK_AP    "d4unlock_append()"
   #define E4_D4UNLOCK_DATA  "d4unlock_data()"
   #ifdef S4VBASIC
      #define E4_D4UNLOCK_FILE  "d4unlockFile()"
      #define E4_D4UNLOCK_FILES "d4unlockFiles()"
   #else
      #define E4_D4UNLOCK_FILE  "d4unlock_file()"
      #define E4_D4UNLOCK_FILES "d4unlock_files()"
   #endif
   #define E4_D4UNLOCK_INDEX "d4unlock_index()"
   #define E4_D4UNLOCK_REC   "d4unlock_records()"
   #define E4_D4UPDATE       "d4update()"
   #define E4_D4UPDATE_HDR   "d4update_header()"
   #define E4_D4UPDATE_PRIO  "d4update_priority()"
   #define E4_D4UPDATE_REC   "d4update_record()"
   #define E4_D4VALID_MID    "d4validate_memo_ids()" 
   #define E4_D4WRITE        "d4write()"
   #define E4_D4WRITE_DATA   "d4write_data()"
   #define E4_D4WRITE_KEYS   "d4write_keys()"
   #define E4_D4ZAP          "d4zap()"
   #define E4_D4ZAP_DATA     "d4zap_data()"
   #define E4_EXPR4DOUBLE    "expr4double()"
   #define E4_EXPR4EXEC      "expr4execute()"
   #define E4_EXPR4KEY       "expr4key()"
   #define E4_EXPR4LOOKUP    "e4lookup()"
   #define E4_EXPR4PARSE     "expr4parse()"
   #define E4_EXPR4PV        "expr4parse_value()"
   #define E4_EXPR4TRUE      "expr4true()"
   #define E4_EXPR4VARY      "expr4vary()"
   #define E4_FILE_LEN       "filelength()"
   #define E4_FILE4LEN       "file4len()"
   #define E4_FILE4LEN_SET   "file4len_set()"
   #define E4_F4ASSIGN       "f4assign()"
   #ifdef S4VBASIC
      #define E4_F4ASSIGN_CHAR  "f4assignChar()"
      #define E4_F4ASSIGN_DBL   "f4assignDouble()"
      #define E4_F4ASSIGN_FLD   "f4assignField()"
      #define E4_F4ASSIGN_INT   "f4assignInt()"
      #define E4_F4ASSIGN_LONG  "f4assignLong()"
      #define E4_F4ASSIGN_N     "f4assignN()"
   #else
      #define E4_F4ASSIGN_CHAR  "f4assign_char()"
      #define E4_F4ASSIGN_DBL   "f4assign_double()"
      #define E4_F4ASSIGN_FLD   "f4assign_field()"
      #define E4_F4ASSIGN_INT   "f4assign_int()"
      #define E4_F4ASSIGN_LONG  "f4assign_long()"
      #define E4_F4ASSIGN_N     "f4assign_n()"
   #endif
   #define E4_F4ASSIGN_PTR   "f4assign_ptr()"
   #define E4_F4BLANK        "f4blank()"
   #define E4_F4CHAR         "f4char()"
   #define E4_F4CREATE       "file4create()"
   #define E4_F4DATA         "f4data()"
   #define E4_F4DECIMALS     "f4decimals()"
   #define E4_F4DOUBLE       "f4double()"
   #define E4_F4DOUBLE2      "f4double2()"
   #define E4_F4FLUSH        "file4flush()"
   #define E4_F4INT          "f4int()"
   #define E4_F4LAG_AND      "f4flag_and()"
   #define E4_F4LAG_OR       "f4flag_or()"
   #define E4_F4LAG_IS_SET   "f4flag_is_set()"
   #define E4_F4LAG_RESET    "f4flag_reset()"
   #define E4_F4LAG_SET      "f4flag_set()"
   #define E4_F4LEN          "f4len()"
   #define E4_F4LOCK         "file4lock()"
   #define E4_F4LONG         "file4long()"
   #define E4_F4LOW_FLUSH    "file4low_flush()"
   #ifdef S4VBASIC
      #define E4_F4MEMO_ASS_N   "f4memoAssignN()"
      #define E4_F4MEMO_ASSIGN  "f4memoAssign()"
   #else
      #define E4_F4MEMO_ASS_N   "f4memo_assign_n()"
      #define E4_F4MEMO_ASSIGN  "f4memo_assign()"
   #endif
   #define E4_F4MEMO_CHECK   "f4memo_check()"
   #define E4_F4MEMO_FLUSH   "f4memo_flush()"
   #ifdef S4VBASIC
      #define E4_F4MEMO_FREE    "f4memoFree()"
      #define E4_F4MEMO_LEN     "f4memoLen()"
   #else
      #define E4_F4MEMO_FREE    "f4memo_free()"
      #define E4_F4MEMO_LEN     "f4memo_len()"
   #endif

   #define E4_F4MEMO_NCPY    "f4memo_ncpy()"
   #define E4_F4MEMO_PTR     "f4memo_ptr()"
   #define E4_F4MEMO_READ    "f4memo_read()"
   #define E4_F4MEMO_READ_LW "f4memo_read_low()"
   #define E4_F4MEMO_RESET   "f4memo_reset()"
   #define E4_F4MEMO_SET_LEN "f4memo_set_len()"
   #ifdef S4VBASIC
      #define E4_F4MEMO_STR     "f4memoStr()"
   #else
      #define E4_F4MEMO_STR     "f4memo_str()"
   #endif

   #define E4_F4MEMO_UPDATE  "f4memo_update()"
   #define E4_F4MEMO_WRITE   "f4memo_write()"
   #define E4_F4NAME         "f4name()"
   #define E4_F4NCY          "f4ncpy()"
   #define E4_F4OPEN         "file4open()"
   #define E4_F4OPTIMIZE     "file4optimize()"
   #define E4_F4OPTIMIZE_WR  "file4optimize_write()"
   #define E4_F4PTR          "f4ptr()"
   #define E4_F4READ         "file4read()"
   #define E4_F4READ_ALL     "file4read_all()"
   #define E4_F4REFRESH      "file4refresh()"
   #define E4_F4REPLACE      "file4replace()"
   #define E4_F4STR          "file4str()"
   #define E4_F4TEMP         "file4temp()"
   #define E4_F4SEQ_READ     "file4seq_read()"
   #define E4_F4SEQ_READ_ALL "file4seq_read_all()"
   #define E4_F4SEQ_READ_IN  "file4seq_read_init()"
   #define E4_F4SEQ_WRITE    "file4seq_write()"
   #define E4_F4SEQ_WRT_FLSH "file4seq_write_flush()"
   #define E4_F4SEQ_WRT_INIT "file4seq_write_init()"
   #define E4_F4SEQ_WRT_REP  "file4seq_write_repeat()"
   #define E4_F4TRUE         "f4true()"
   #define E4_F4TYPE         "f4type()"
   #define E4_F4UNLOCK       "file4unlock()"
   #define E4_F4WRITE        "file4write()"
   #define E4_I4ADD_TAG      "i4add_tag()"
   #define E4_I4CHECK        "i4check()"
   #define E4_I4CLOSE        "i4close()"
   #define E4_I4CREATE       "i4create()"
   #define E4_I4EXTEND       "i4extend()"
   #define E4_I4FLUSH        "i4flush()"
   #define E4_I4GET_LAST_KEY "i4get_last_key()"
   #define E4_I4LOCK         "i4lock()"
   #define E4_I4OPEN         "i4open()"
   #define E4_I4READ_BLOCK   "i4read_block()"
   #define E4_I4REINDEX      "i4reindex()"
   #define E4_I4REINDEX_ADD  "r4reindex_add()"
   #define E4_I4REINDEX_BA   "r4reindex_blocks_alloc()"
   #define E4_I4REINDEX_FN   "r4reindex_finish()"
   #define E4_I4REINDEX_SK   "r4reindex_supply_keys()"
   #define E4_I4REINDEX_TD   "r4reindex_todisk()"
   #define E4_I4REINDEX_THW  "r4reindex_tag_headers_write()"
   #define E4_I4REINDEX_THWS "r4reindex_tag_headers_write_special()"
   #define E4_I4REINDEX_WK   "r4reindex_write_keys()"
   #define E4_I4SHRINK       "i4shrink()"
   #define E4_I4TAG          "i4tag()"
   #define E4_I4TAG_INFO     "i4tag_info()"
   #define E4_I4UNLOCK       "i4unlock()"
   #define E4_I4UPDATE       "i4update()"
   #define E4_I4UPDATE_HDR   "i4update_header()"
   #define E4_I4VERSION_CHK  "i4version_check()"
   #define E4_LOG4TRUE       "log4true()"
   #define E4_L4ADD          "l4add()"
   #define E4_L4ADD_AFTER    "l4add_after()"
   #define E4_L4ADD_BEFORE   "l4add_before()"
   #define E4_L4CHECK        "l4check()"
   #define E4_L4FIRST        "l4first()"
   #define E4_L4LAST         "l4last()"
   #define E4_L4NEXT         "l4next()"
   #define E4_L4POP          "l4pop()"
   #define E4_L4PREV         "l4prev()"
   #define E4_L4REMOVE       "l4remove()"
   #define E4_MEMO4FILE_CR   "memo4file_create()"
   #define E4_MEMO4FILE_RD   "memo4file_read()"
   #define E4_MEMO4FILE_WR   "memo4file_write()"
   #define E4_MEM4ALLOC      "mem4alloc()"
   #define E4_MEM4CREATE     "mem4create()"
   #define E4_MEM4FREE       "mem4free()"
   #define E4_OPT4BLOCK_ADD  "opt4block_add()"
   #define E4_OPT4BLOCK_CLR  "opt4block_clear()"
   #define E4_OPT4BLOCK_FLSH "opt4block_flush()"
   #define E4_OPT4BLOCK_REM  "opt4block_remove()"
   #define E4_OPT4FILE_CH    "opt4file_choose_block()"
   #define E4_OPT4FILE_DEL   "opt4file_delete()"
   #define E4_OPT4FILE_FL    "opt4file_find_list()"
   #define E4_OPT4FILE_GB    "opt4file_get_block()"
   #define E4_OPT4FILE_LRUB  "opt4file_lru_bottom()"
   #define E4_OPT4FILE_LRUT  "opt4file_lru_top()"
   #define E4_OPT4FILE_RB    "opt4file_return_block()"
   #define E4_OPT4FILE_RD    "opt4file_read()"
   #define E4_OPT4FILE_RD_FL "opt4file_read_file()"
   #define E4_OPT4FILE_RD_SB "opt4file_read_sp_buffer()"
   #define E4_OPT4FILE_WR    "opt4file_write()"
   #define E4_OPT4FLUSH_ALL  "opt4flush_all()"
   #define E4_OPT4FLUSH_LST  "opt4flush_lst()"
   #define E4_OPT4FLUSH_WB   "opt4flush_write_buffer()"
   #define E4_OPT4FREE       "opt4free_alloc()"
   #define E4_OPT4INIT       "opt4free_init()"
   #define E4_OPT4SET_PRIO   "opt4free_set_priority()"
   #define E4_R4BOTTOM       "relate4bottom()"
   #define E4_R4CHANGED      "relate4changed()"
   #ifdef S4VBASIC
      #define E4_R4CREATE_SLAVE "relate4createSlave()"
   #else
      #define E4_R4CREATE_SLAVE "relate4create_slave()"
   #endif
   #define E4_R4DO           "relate4do()"
   #ifdef S4VBASIC
      #define E4_R4DO_ONE       "relate4doOne()"
   #else
      #define E4_R4DO_ONE       "relate4do_one()"
   #endif
   #define E4_R4EOF          "relate4eof()"
   #define E4_R4FREE         "relate4free()" 
   #define E4_R4INIT         "relate4init()"
   #define E4_R4LOOKUP       "relate4lookup()"
   #define E4_R4LOCK         "relate4lock()"
   #ifdef S4VBASIC
      #define E4_R4MATCH_LEN    "relate4matchLen()"
   #else
      #define E4_R4MATCH_LEN    "relate4match_len()"
   #endif
   #define E4_R4NEXT         "relate4next()"
   #define E4_R4NEXT_RIS     "relate4next_record_in_scan()"
   #define E4_R4PREV_RIS     "relate4prev_record_in_scan()"
   #ifdef S4VBASIC
      #define E4_R4QUERY_SET    "relate4querySet()"
   #else
      #define E4_R4QUERY_SET    "relate4query_set()"
   #endif
   #define E4_R4READ_REST    "relate4read_rest()"
   #define E4_R4SKIP         "relate4skip()"
   #define E4_R4SORT         "relate4sort()"
   #ifdef S4VBASIC
      #define E4_R4SORT_SET     "relate4sortSet()"
   #else
      #define E4_R4SORT_SET     "relate4sort_set()"
   #endif
   #define E4_R4TOP          "relate4top()"
   #define E4_R4UNLOCK       "relate4unlock()"
   #define E4_S4GET          "sort4get()"
   #define E4_S4GET_INIT     "sort4get_init()"
   #define E4_S4GET_MEM_INIT "sort4get_mem_init()"
   #define E4_S4INIT         "sort4init()"
   #define E4_S4SPOOLS_INIT  "sort4spools_init()"
   #define E4_S4NEXT_SP_ENT  "s4next_spool_entry()"
   #define E4_S4PUT          "sort4put()"
   #define E4_T4ADD          "t4add()"
   #define E4_T4ADD_CALC     "t4add_calc()"
   #define E4_T4BALANCE_BR   "t4balance_branch()"
   #define E4_T4BLOCK        "t4block()"
   #define E4_T4BLOCK_CHK    "t4block_check()"
   #define E4_T4BOTTOM       "t4bottom()"
   #define E4_T4CHECK        "t4check()"
   #define E4_T4CLOSE        "t4close()"
   #define E4_T4CREATE       "t4create()"
   #define E4_T4DOWN         "t4down()"
   #define E4_T4DO_VERSION   "t4do_version_check()"
   #define E4_T4DUMP         "t4dump"
   #define E4_T4EOF          "t4eof()"
   #define E4_T4EXTEND       "t4extend()"
   #define E4_T4FLUSH        "t4flush()"
   #define E4_T4FREE_ALL     "t4free_all()"
   #define E4_T4GO           "t4go()"
   #define E4_T4INIT         "t4init()"
   #define E4_T4KEY          "t4key()"
   #define E4_T4OPEN         "t4open()"
   #define E4_T4POSITION     "t4position()"
   #define E4_T4POSITION2    "t4position2()"
   #ifdef S4VBASIC
      #define E4_T4POSITION_SET "t4positionSet()"
   #else
      #define E4_T4POSITION_SET "t4position_set()"
   #endif
   #define E4_T4RECNO        "t4recno()"
   #define E4_T4REMOVE       "t4remove()"
   #define E4_T4REM_CALC     "t4remove_calc()"
   #define E4_T4SEEK         "t4seek()"
   #define E4_T4SKIP         "t4skip()"
   #define E4_T4TOP          "t4top()"
   #define E4_T4UP           "t4up()"
   #define E4_T4UPDATE       "t4update()"
   #define E4_T4UP_TO_ROOT   "t4up_to_root()"
   #define E4_U4ALLOC_AGN    "u4alloc_again()"
   #define E4_U4NAME_EXT     "u4name_ext()"
   #define E4_U4NAME_PIECE   "u4name_piece()"
   #define E4_U4NAME_RET_EXT "u4name_ret_ext()"
   #define E4_U4NCPY         "u4ncpy()"
#endif

#ifdef S4OFF_ERROR

   /* e4() messages */
   #define  E4_CLOSE         0
   #define  E4_CREATE_FIL    0
   #define  E4_CREATE_TEM    0
   #define  E4_CREATE_TOO    0
   #define  E4_ERROR         0
   #define  E4_ERROR_NUM     0
   #define  E4_ERROR_SEV     0
   #define  E4_ERROR_BAS     0
   #define  E4_ERROR_COD     0
   #define  E4_ERROR_CDS     0
   #define  E4_ERROR_ENT     0
   #define  E4_ERROR_KEY     0
   #define  E4_INDEX_COR     0
   #define  E4_INDEX_EXP     0
   #define  E4_INDEX_FIL     0
   #define  E4_INFO_AME      0
   #define  E4_INFO_BAC      0
   #define  E4_INFO_CAL      0
   #define  E4_INFO_CLO      0
   #define  E4_INFO_DAT      0
   #define  E4_INFO_EMF      0
   #define  E4_INFO_IFT      0
   #define  E4_INFO_INC      0
   #define  E4_INFO_KEY      0
   #define  E4_INFO_LOC      0
   #define  E4_INFO_REA      0
   #define  E4_INFO_REC      0
   #define  E4_INFO_REL      0
   #define  E4_INFO_REP      0
   #define  E4_INFO_SET      0
   #define  E4_INFO_SKI      0
   #define  E4_INFO_SKL      0
   #define  E4_INFO_TAG      0
   #define  E4_LOCK_LOC      0
   #define  E4_LOCK_UNL      0
   #define  E4_MEMORY_B      0
   #define  E4_MEMORY_S      0
   #define  E4_NUM_PARMS     0
   #define  E4_OPT_INV       0
   #define  E4_RESULT_CII    0
   #define  E4_RESULT_D4F    0
   #define  E4_RELATE_EAI    0
   #define  E4_RELATE_MEN    0
   #define  E4_RELATE_REL    0
   #define  E4_RELATE_RCS    0
   #define  E4_REPORT_FILE   0
   #define  E4_REPORT_ONE    0
   #define  E4_REPORT_DFILE  0
   #define  E4_REPORT_GRO    0
   #define  E4_REPORT_IFILE  0
   #define  E4_REPORT_ASTYLE 0
   #define  E4_REPORT_AGROUP 0
   #define  E4_REPORT_RGROUP 0
   #define  E4_REPORT_ATOTAL 0
   #define  E4_REPORT_ATEXT  0
   #define  E4_REPORT_HINST  0
   #define  E4_REPORT_HWND   0
   #define  E4_REPORT_RTEXT  0
   #define  E4_REPORT_RCALC  0
   #define  E4_REPORT_RTOTAL 0
   #define  E4_REPORT_TRG    0
   #define  E4_REPORT_ROBJS  0
   #define  E4_RESULT_DAT    0
   #define  E4_RESULT_EXP    0
   #define  E4_RESULT_IDE    0
   #define  E4_RESULT_INT    0
   #define  E4_RESULT_LCF    0
   #define  E4_RESULT_LOS    0
   #define  E4_RESULT_REC    0
   #define  E4_RESULT_TAG    0
   #define  E4_RESULT_THE    0
   #define  E4_RESULT_UNE    0
   #define  E4_RESULT_WAS    0
   #define  E4_PARM_MUS      0
   #define  E4_PARM_OPT      0
   #define  E4_PARM_REL      0
   #define  E4_PARM_SIZ      0
   #define  E4_PARM_REP      0
   #define  E4_TYPE_SUB      0
   #define  E4_PARM_TAA      0
   #define  E4_TYPE_UFT      0
   #define  E4_PARM_WRT      0
   #define  E4_UNIQUE        0

   /* e4severe() messages */
   #define  E4_BM4_IEL       0
   #define  E4_BM4_IM        0
   #define  E4_CONST_EIN     0
   #define  E4_DATA_ILL      0
   #define  E4_DATA_COR      0
   #define  E4_DATA_DEL      0
   #define  E4_DATA_DELD     0
   #define  E4_DATA_MEM      0
   #define  E4_DATA_RECALL   0
   #define  E4_DATA_UPDATE   0
   #define  E4_EXPR_DELETED  0
   #define  E4_INFO_ACC      0
   #define  E4_INFO_ALR      0
   #define  E4_INFO_BDC      0
   #define  E4_INFO_BDI      0
   #define  E4_INFO_BLO      0
   #define  E4_INFO_BLS      0
   #define  E4_INFO_BMC      0
   #define  E4_INFO_C4C      0
   #define  E4_INFO_COD      0
   #define  E4_INFO_CIF      0
   #define  E4_INFO_CIB      0
   #define  E4_INFO_CIS      0
   #define  E4_INFO_CMF      0
   #define  E4_INFO_CRL      0
   #define  E4_INFO_DAO      0
   #define  E4_INFO_DEL      0
   #define  E4_INFO_DUP      0
   #define  E4_INFO_EPV      0
   #define  E4_INFO_EVA      0
   #define  E4_INFO_EXK      0
   #define  E4_INFO_EXP      0
   #define  E4_INFO_FAI      0
   #define  E4_INFO_FOR      0
   #define  E4_INFO_ILF      0
   #define  E4_INFO_ILL      0
   #define  E4_INFO_ILP      0
   #define  E4_INFO_IMS      0
   #define  E4_INFO_INT      0
   #define  E4_INFO_INV      0
   #define  E4_INFO_IAO      0
   #define  E4_INFO_IVE      0
   #define  E4_INFO_IVT      0
   #define  E4_INFO_LIN      0
   #define  E4_INFO_NKE      0
   #define  E4_INFO_STR      0
   #define  E4_INFO_TAO      0
   #define  E4_INFO_UNE      0
   #define  E4_INFO_UNI      0
   #define  E4_INFO_UNT      0
   #define  E4_INFO_WRO      0
   #define  E4_INFO_WT4      0
   #define  E4_MEMORY_ERR    0
   #define  E4_MEMORY_OOR    0
   #define  E4_MEMORY_SAV    0
   #define  E4_MEMORY_YPO    0
   #define  E4_MEMORY_YPU    0
   #define  E4_OPEN_NOD      0
   #define  E4_PARM_FLA      0
   #define  E4_PARM_FOR      0
   #define  E4_PARM_ILP      0
   #define  E4_PARM_IND      0
   #define  E4_PARM_INV      0
   #define  E4_PARM_NFD      0
   #define  E4_PARM_NUL      0
   #define  E4_PARM_NSD      0
   #define  E4_PARM_OSD      0
   #define  E4_PARM_REC      0
   #define  E4_PARM_TAG      0
   #define  E4_PARM_TOO      0
   #define  E4_PARM_UNI      0
   #define  E4_PARM_ZER      0
   #define  E4_RESULT_CLI    0
   #define  E4_RESULT_COM    0
   #define  E4_RESULT_COR    0
   #define  E4_RESULT_D4A    0
   #define  E4_RESULT_D4I    0
   #define  E4_RESULT_EXI    0
   #define  E4_RESULT_FRE    0
   #define  E4_RESULT_INC    0
   #define  E4_RESULT_INQ    0
   #define  E4_RESULT_LOC    0
   #define  E4_RESULT_LCO    0
   #define  E4_RESULT_MEM    0
   #define  E4_RESULT_REM    0
   #define  E4_RESULT_S4L    0
   #define  E4_RESULT_STC    0
   #define  E4_RESULT_TOO    0

   #ifdef S4WINDOWS
      #define E4_REPORT_BIT     0
      #define E4_REPORT_COD     0
      #define E4_REPORT_CRE     0
      #define E4_REPORT_DIS     0
      #define E4_REPORT_FON     0
      #define E4_REPORT_PRI     0
      #define E4_RESULT_CRE     0
      #define E4_RESULT_DEL     0
      #define E4_RESULT_END     0
      #define E4_RESULT_G4D     0
      #define E4_RESULT_G4E     0
      #define E4_RESULT_G4L     0
      #define E4_RESULT_ON      0
      #define E4_RESULT_SET     0
      #define E4_RESULT_STA     0
      #define E4_MESSAG_EXI     0
      #define E4_MESSAG_CAN     0
      #define E4_MESSAG_OK      0
      #define E4_MESSAG_PAC     0
      #define E4_MESSAG_REC     0
      #define E4_MESSAG_REI     0
      #define E4_PARM_ADD       0
      #define E4_PARM_SA        0
      #define E4_PARM_VIS       0
      #define E4_PARM_WIN       0
      #define E4_RESULT_ADD     0
      #define E4_RESULT_BUF     0
      #define E4_RESULT_CDC     0
      #define E4_RESULT_ENW     0
      #define E4_RESULT_GDC     0
      #define E4_RESULT_GTM     0
      #define E4_RESULT_ID      0
      #define E4_RESULT_IDB     0
      #define E4_RESULT_IDC     0
      #define E4_RESULT_IDD     0
      #define E4_RESULT_IDL     0
      #define E4_RESULT_IDT     0
      #define E4_RESULT_PIC     0
      #define E4_RESULT_POS     0
      #define E4_RESULT_RDC     0
      #define E4_RESULT_RES     0
      #define E4_RESULT_TA1     0
      #define E4_RESULT_TA2     0
      #define E4_RESULT_WIN     0
   #endif

#else
#ifndef S4LANGUAGE

   /* e4() messages */
   #define  E4_CLOSE         "Unknown Name"
   #define  E4_CREATE_FIL    "File Name:"
   #define  E4_CREATE_TEM    "Temporary Working File"
   #define  E4_CREATE_TOO    "Too Many Fields"
   #define  E4_ERROR         "Error"
   #define  E4_ERROR_NUM     "\r\n\r\nError Number"
   #define  E4_ERROR_SEV     "\r\n\r\nSevere Error Number"
   #define  E4_ERROR_BAS     "CODEBASIC ERROR"
   #define  E4_ERROR_COD     "CODEBASE ERROR"
   #define  E4_ERROR_CDS     "CODEBASE SEVERE ERROR"
   #define  E4_ERROR_ENT     "\r\nPress Enter ..."
   #define  E4_ERROR_KEY     "\r\nPress a key ..."
   #define  E4_EXPR_OM       "expression source length > max allowable chars for index"
   #define  E4_INDEX_COR     "Corrupted Free Block Chain in Index File"
   #define  E4_INDEX_EXP     "Expected '.CDX' file is of the '.IDX' type"
   #define  E4_INDEX_FIL     "Filter should be Logical"
   #define  E4_INFO_AME      "A memo block is used twice."
   #define  E4_INFO_BAC      "Backwards skip attempted without calling relate4skip_enable()"
   #define  E4_INFO_CAL      "Call to d4reccount() in r4reindex_blocks_alloc() failed"
   #define  E4_INFO_CLO      "Error closing group file in i4open()"
   #define  E4_INFO_DAT      "Database is empty."
   #define  E4_INFO_EMF      "Expected memo field non-existant in f4memo_read_low()"
   #define  E4_INFO_IFT      "f4assign_field() - invalid field type"
   #define  E4_INFO_INC      "Incorrect Record Number in tag:"
   #define  E4_INFO_KEY      "Error in Key Evaluation"
   #define  E4_INFO_LOC      "Error locking in i4open()"
   #define  E4_INFO_REA      "Error reading group file in i4open()"
   #define  E4_INFO_REC      "Record is missing."
   #define  E4_INFO_REL      "relate4top() must be called first"
   #define  E4_INFO_REP      "Repeated Record Number in tag:"
   #define  E4_INFO_SET      "Set CODE4.auto_open to 'off'"
   #define  E4_INFO_SKL      "CODE4->stored_key_len too small"
   #define  E4_INFO_SKI      "Skip must be made from valid record number"
   #define  E4_INFO_TAG      "Tag is corrupted"
   #define  E4_LOCK_LOC      "Attempt to lock file that is read-only"
   #define  E4_LOCK_UNL      "Attempt to unlock file that is read-only"
   #define  E4_MEMORY_B      "Block Size too Large for Indexing"
   #define  E4_MEMORY_S      "Sorting"
   #define  E4_NUM_PARMS     "Function"
   #define  E4_OPT_INV       "Invalid mode"
   #define  E4_RESULT_CII    "Clipper incompatible Index File detected."
   #define  E4_RESULT_D4F    "d4flush_record() did not work."
   #define  E4_RELATE_EAI    "RELATE4->error_action invalid"
   #define  E4_RELATE_MEN    "RELATE4->master expected is null"
   #define  E4_RELATE_REL    "relate4match_len() can only be called with a character expression"
   #define  E4_RELATE_RCS    "relate4create_slave() - master expression type does not match tag type"
   #define  E4_REPORT_FILE   "This file is not a CodeReporter report file."
   #define  E4_REPORT_ONE    "Only one report can be loaded at a time"
   #define  E4_REPORT_DFILE  "Unable to Open Data File"
   #define  E4_REPORT_GRO    "Group Not Found while Retrieving Report"
   #define  E4_REPORT_IFILE  "Index File Not Found"
   #define  E4_REPORT_ASTYLE "Unable to Allocate Memory For Style"
   #define  E4_REPORT_AGROUP "Unable to Allocate Memory For Group"
   #define  E4_REPORT_RGROUP "Unable to Retrieve Group Information"
   #define  E4_REPORT_ATOTAL "Unable to Allocate Memory for Total"
   #define  E4_REPORT_ATEXT  "Unable to Allocate Memory for Text Object"
   #define  E4_REPORT_HINST  "'hInst' member of CODE4 is not Initialized"
   #define  E4_REPORT_HWND   "'hWndParent' member of REPORT4 is not Initialized"
   #define  E4_REPORT_RTEXT  "Unable to Retrieve Text Object Information"
   #define  E4_REPORT_RCALC  "Unable to Retrieve Calculation Information"
   #define  E4_REPORT_RTOTAL "Unable to Retrieve Total Information"
   #define  E4_REPORT_TRG    "Unable to Locate Reset Group"
   #define  E4_REPORT_ROBJS  "Unable to Retrieve Objects Information"
   #define  E4_RESULT_DAT    "Database not found"
   #define  E4_RESULT_EXP    "Expecting logical result."
   #define  E4_RESULT_IDE    "Identical keys in unique key tag:"
   #define  E4_RESULT_INT    "Internal buffer too small while retrieving report" 
   #define  E4_RESULT_LCF    "Could not locate File:"
   #define  E4_RESULT_LOS    "Lost Block in Index File"
   #define  E4_RESULT_REC    "Record numbers out of order for equal keys:"
   #define  E4_RESULT_TAG    "Tag is out of date:"
   #define  E4_RESULT_THE    "The tag is out of order:"
   #define  E4_RESULT_UNE    "Unexpected key evaluation length:"
   #define  E4_RESULT_WAS    "Wasted Space in Memo File"
   #define  E4_PARM_MUS      "relate4top() must be called first"
   #define  E4_PARM_OPT      "CODE4 optimization initialization values too small" 
   #define  E4_PARM_REL      "Called relate4create_slave() with a database that already exists in another part of the relation" 
   #define  E4_PARM_REP      "Attempt to replace to a read-only file" 
   #define  E4_PARM_SIZ      "Attempt to change file length of a read-only file"
   #define  E4_PARM_TAA      "i4add_tag() - an input tag already exists in the index file"
   #define  E4_PARM_WRT      "Attempt to write to a read-only file" 
   #define  E4_TYPE_SUB      "Expression:"
   #define  E4_TYPE_UFT      "Unknown field type"
   #define  E4_UNIQUE        "Creating Tag"

   /* e4severe() messages */
   #define  E4_BM4_IEL       "bitmap4seek() - invalid expression length"
   #define  E4_BM4_IM        "bitmap4combine_leafs() - incompatible maps"
   #define  E4_CONST_EIN     "const4get() - expression incongruity"
   #define  E4_DATA_ILL      "Illegal field data."
   #define  E4_DATA_COR      "Corrupt Database File."
   #define  E4_DATA_DEL      "d4delete() - invalid deletion flag detected"
   #define  E4_DATA_DELD     "d4deleted() - invalid deletion flag detected"
   #define  E4_DATA_RECALL   "d4recall() - invalid deletion flag"
   #define  E4_DATA_MEM      "Memo File not Open for Append"
   #define  E4_DATA_UPDATE   "Number records unknown at update time"
   #define  E4_EXPR_DELETED  "e4deleted() - invalid value detected in record buffer's deleted flag"
   #define  E4_INFO_ACC      "Accessing NULL Block"
   #define  E4_INFO_ALR      "Already initialized"
   #define  E4_INFO_BDC      "Corrupt NTX Clipper index file"
   #define  E4_INFO_BDI      "Corrupt IDX FoxPro index file"
   #define  E4_INFO_BLO      "Block size is less than 1024 bytes"
   #define  E4_INFO_BLS      "This is unsupported since dBASE IV compatibility requires minimum blocks sizes of 1024 bytes"
   #define  E4_INFO_BMC      "Block/memory corruption"
   #define  E4_INFO_C4C      "'CODE4.mem_size_block' is an illegal value."
   #define  E4_INFO_COD      "CodeBase internal error"
   #define  E4_INFO_CIF      "Corrupt index file"
   #define  E4_INFO_CIB      "Corrupt index block"
   #define  E4_INFO_CIS      "Corrupt index structure"
   #define  E4_INFO_CMF      "Corrupt memo file"
   #define  E4_INFO_CRL      "Corrupted Linked List"
   #define  E4_INFO_DAO      "Database file already open"
   #define  E4_INFO_DEL      "Deleted block not empty"
   #define  E4_INFO_DUP      "Duplicate Alias"
   #define  E4_INFO_EPV      "Expected value not built"
   #define  E4_INFO_EVA      "Evaluating Tag Expression"
   #define  E4_INFO_EXK      "Expression key is larger than maximum 102 characters"
   #define  E4_INFO_EXP      "Expression too large"
   #define  E4_INFO_FAI      "Failure in logical field index"
   #define  E4_INFO_FOR      "For tag"
   #define  E4_INFO_ILF      "Illegal file block reference"
   #define  E4_INFO_ILL      "Illegal locking configuration"
   #define  E4_INFO_ILP      "Illegal position"
   #define  E4_INFO_IMS      "Invalid memo size in memo4file_write()"
   #define  E4_INFO_INT      "Internal t4remove_current() failure--index file corrupt"
   #define  E4_INFO_INV      "Invalid key type in index file"
   #define  E4_INFO_IAO      "Index file already open"
   #define  E4_INFO_IVE      "Invalid error action"
   #define  E4_INFO_IVT      "Invalid type"
   #define  E4_INFO_LIN      "Link not on list"
   #define  E4_INFO_NKE      "n_keys invalid"
   #define  E4_INFO_STR      "Function was Passed an Invalid Structure Pointer"
   #define  E4_INFO_TAO      "Tag file already open"
   #define  E4_INFO_UNE      "Unexpected empty block"
   #define  E4_INFO_UNI      "Un-initialized values in opt4add_block"
   #define  E4_INFO_UNT      "Unexpected tag locks in index file"
   #define  E4_INFO_WRO      "Wrong number of links in linked list"
   #define  E4_INFO_WT4      "Wrong TAG4 Type"
   #define  E4_MEMORY_ERR    "Memory Error"
   #define  E4_MEMORY_OOR    "Memory out of range in const4mem_alloc()"
   #define  E4_MEMORY_SAV    "l4lock_save(), Saving Lock Information"
   #define  E4_MEMORY_YPO    "mem4pop_pointer() with S4DEBUG switch."
   #define  E4_MEMORY_YPU    "mem4push_pointer() with S4DEBUG switch."
   #define  E4_OPEN_NOD      "i4open(): No database specified"
   #define  E4_PARM_FLA      "Flag should be 'r4descending'."
   #define  E4_PARM_FOR      "For S4NDX version, filter and descending should be 0"
   #define  E4_PARM_ILP      "Illegal position"
   #define  E4_PARM_IND      "Index file already open"
   #define  E4_PARM_INV      "Invalid lock position"
   #define  E4_PARM_NFD      "relate4sort(): Non-freed data list"
   #define  E4_PARM_NUL      "Null Data File Pointer"
   #define  E4_PARM_NSD      "Null String Detected"
   #define  E4_PARM_OSD      "Overlapping Strings Detected"
   #define  E4_PARM_REC      "Record Numbers are not Consecutive"
   #define  E4_PARM_TAG      "Tag expression is missing."
   #define  E4_PARM_TOO      "Too many tags"
   #define  E4_PARM_UNI      "Unique flag data is an unrecognized value."
   #define  E4_PARM_ZER      "Zero parameter"
   #define  E4_RESULT_CLI    "Clipper Key Conversion failed"
   #define  E4_RESULT_CMP    "mem4check_pointer() - corrupt memory pointer encountered"
   #define  E4_RESULT_COM    "Corrupted Memory Detected"
   #define  E4_RESULT_COR    "Corrupted Index File in Check"
   #define  E4_RESULT_D4A    "d4append_start() was not completed."
   #define  E4_RESULT_D4I    "d4init() has not been called."
   #define  E4_RESULT_EXI    "Exiting from within lock wait"
   #define  E4_RESULT_FRE    "Free value not found"
   #define  E4_RESULT_INC    "Incorrect index file locking order."
   #define  E4_RESULT_INQ    "In quick sort."
   #define  E4_RESULT_LOC    "Locking index could create deadlock."
   #define  E4_RESULT_LCO    "Locking Overlap Detected"
   #define  E4_RESULT_MEM    "Memory items not freed"
   #define  E4_RESULT_REM    "Removing Lock which was never placed."
   #define  E4_RESULT_S4L    "S4LOCK_CHECK cannot be used with 'mem4reset'"
   #define  E4_RESULT_STC    "Stack length variable _stklen below minimum."
   #define  E4_RESULT_TOO    "Too many pointers"

   #ifdef S4WINDOWS
      #define E4_REPORT_BIT     "Could not create a bitmap"
      #define E4_REPORT_COD     "CodeReporter"
      #define E4_REPORT_CRE     "Could not create output window"
      #define E4_REPORT_DIS     "Could not get a display context"
      #define E4_REPORT_FON     "Error Creating Font"
      #define E4_REPORT_PRI     "Printing"
      #define E4_RESULT_CRE     "Creating Printer Display Context"
      #define E4_RESULT_DEL     "Deleting Display Context"
      #define E4_RESULT_END     "Ending Document"
      #define E4_RESULT_G4D     "G4DISPLAY buffer missing"
      #define E4_RESULT_G4E     "G4EDIT buffer missing"
      #define E4_RESULT_G4L     "G4LIST buffer missing"
      #define E4_RESULT_ON      "On New Frame"
      #define E4_RESULT_SET     "Setting Font"
      #define E4_RESULT_STA     "Starting Print Document"
      #define E4_MESSAG_EXI     "EXITING APPLICATION"
      #define E4_MESSAG_CAN     "Cancel"
      #define E4_MESSAG_OK      "OK"
      #define E4_MESSAG_PAC     "Are you sure you want to pack ?"
      #define E4_MESSAG_REC     "RECORD NUMBER"
      #define E4_MESSAG_REI     "Are you sure you want to reindex ?"
      #define E4_PARM_ADD       "Choice length must be > 0"
      #define E4_PARM_SA        "Invalid G4DISPLAY Pointer"
      #define E4_PARM_VIS       "WS_VISIBLE Style not Present"
      #define E4_PARM_WIN       "Invalid Window Dimensions"
      #define E4_RESULT_ADD     "Add String Failed"
      #define E4_RESULT_BUF     "G4LIST Buffer Not Big Enough"
      #define E4_RESULT_CDC     "CreateCompatibleDC() Failed"
      #define E4_RESULT_ENW     "EnableWindow() Failed"
      #define E4_RESULT_GDC     "GetDC() Failed"
      #define E4_RESULT_GTM     "GetTextMetrics Failed"
      #define E4_RESULT_ID      "Invalid Id Number"
      #define E4_RESULT_IDB     "Not A G4BUTTON Id Number"
      #define E4_RESULT_IDC     "Not A G4COMBO Id Number"
      #define E4_RESULT_IDD     "Not A G4DISPLAY Id Number"
      #define E4_RESULT_IDL     "Not A G4LIST Id Number"
      #define E4_RESULT_IDT     "Not A G4EDIT Id Number"
      #define E4_RESULT_PIC     "Bad Picture"
      #define E4_RESULT_POS     "Bad Caret Position"
      #define E4_RESULT_RDC     "ReleaseDC() Failed"
      #define E4_RESULT_RES     "FindResource() Failed"
      #define E4_RESULT_TA1     "No WM_TABSTOP Style Present"
      #define E4_RESULT_TA2     "Control Cannot Have The WM_TABSTOP Style"
      #define E4_RESULT_WIN     "CreateWindow() Failed"
   #endif

#else     /*  if S4LANGUAGE is defined  */

#ifdef S4GERMAN

      #define  E4_CLOSE            "Unbekannter Name"
      #define  E4_CREATE_FIL       "Dateiname"
      #define  E4_CREATE_TEM       "Tempor„rdatei"
      #define  E4_CREATE_TOO       "Zu viele Felder vorhanden"
      #define  E4_ERROR            "Fehler"
      #define  E4_ERROR_NUM        "\r\n\r\nFehler Nummer"
      #define  E4_ERROR_SEV        "\r\n\r\nSchwerer Fehler Nummer"
      #define  E4_ERROR_BAS        "CODEBASIC FEHLER"
      #define  E4_ERROR_COD        "CODEBASE FEHLER"
      #define  E4_ERROR_CDS        "CODEBASE SEVERE FEHLER"
      #define  E4_ERROR_ENT        "\r\nReturn-Taste drcken ..."  
      #define  E4_ERROR_KEY        "\r\nEine Taste drcken ..."
      #define  E4_EXPR_OM          "Ausdrucksl„nge zu groá fr diesen Index"
      #define  E4_INDEX_COR        "Besch„digte freie Blockkette in der Indexdatei"
      #define  E4_INDEX_EXP        "Die erwartete '.CDX' Datei ist vom '.IDX' Typ"
      #define  E4_INDEX_FIL        "Der Filter sollte logisch sein"
      #define  E4_INFO_AME         "Ein Memo Block wurde zweimal gentzt"
      #define  E4_INFO_BAC         "Backwards skip attempted without calling relate4skip_enable()"
      #define  E4_INFO_CAL         "Der Aufruf von d4reccount() in r4reindex_blocks init() ist nicht gelungen"
      #define  E4_INFO_CLO         "Fehler bei Abschluá der Gruppendatei in i4open()"
      #define  E4_INFO_DAT         "Datenbank ist leer"
      #define  E4_INFO_EMF         "erwartetes Memofeld nicht vorhanden in f4memo_read_low()"
      #define  E4_INFO_IFT         "f4assign_field() - ungltiger Feld-Typ"
      #define  E4_INFO_INC         "Fehlerhafte Satznummer in 'Tag'"
      #define  E4_INFO_KEY         "Fehler bei der Schlsselauswertung"
      #define  E4_INFO_LOC         "Fehler beim Locken in i4open()"
      #define  E4_INFO_REA         "Fehler beim Lesen der Gruppendatei in i4open()"
      #define  E4_INFO_REC         "Ein Satz fehlt"
      #define  E4_INFO_REL         "relate4top() muá vorher aufgerufen werden"
      #define  E4_INFO_REP         "Wiederholte Satznummer in 'Tag'"
      #define  E4_INFO_SET         "CODE4.auto_open ausschalten"
      #define  E4_INFO_SKL         "CODE4->stored_key_len zu klein"
      #define  E4_INFO_SKI         "Skip muá von gltiger Satznummer erfolgen"
      #define  E4_INFO_TAG         "'Tag' ist besch„digt"
      #define  E4_LOCK_LOC         "Attempt to lock file that is read-only"
      #define  E4_LOCK_UNL         "Attempt to unlock file that is read-only"
      #define  E4_MEMORY_B         "Der Block ist zum Indexieren zu umfangreich"
      #define  E4_MEMORY_S         "Sortieren"
      #define  E4_NUM_PARMS        "Funktion"
      #define  E4_OPT_INV          "Ungltiger Modus"
      #define  E4_REPORT_FILE      "Keine Report-Datei von CodeReporter."
      #define  E4_REPORT_ONE       "Nur ein Report auf einmal ladbar"
      #define  E4_REPORT_DFILE     "Kann Datendatei nicht ”ffnen"
      #define  E4_REPORT_GRO       "Gruppe nicht gefunden bei Report-Auswertung"
      #define  E4_REPORT_IFILE     "Index-Datei nicht gefunden"
      #define  E4_REPORT_ASTYLE    "Kein Speicher fr Schriftart"
      #define  E4_REPORT_AGROUP    "Kein Speicher fr Gruppe"
      #define  E4_REPORT_RGROUP    "Kein Zugriff auf Gruppen-Information"

      #define  E4_REPORT_ATOTAL    "Kein Speicher fr Summenfeld"
      #define  E4_REPORT_ATEXT     "Kein Speicher fr Text-Objekt"
      #define  E4_REPORT_HINST     "CODE4-Mitglied 'hInst' nicht initialisiert"
      #define  E4_REPORT_HWND      "REPORT4-Mitglied 'hWndParent' nicht initialisiert"
      #define  E4_REPORT_RTEXT     "Kein Zugriff auf Information zu Text Objekt"
      #define  E4_REPORT_RCALC     "Kein Zugriff auf Ergebnis der Berechnung"
      #define  E4_REPORT_RTOTAL    "Kein Zugriff auf Summenfeld"
      #define  E4_REPORT_TRG       "Kann Reset-Gruppe nicht lokalisieren"
      #define  E4_REPORT_ROBJS     "Kein Zugriff auf Informationen ber Objekt"
      #define  E4_RESULT_CII       "Nicht kompatible Clipper-Index-Datei entdeckt."
      #define  E4_RESULT_D4F       "d4flush record() gescheitert"
      #define  E4_RESULT_DAT       "Datenbank nicht gefunden"
      #define  E4_RESULT_EXP       "Es werden logische Ergebnisse erwartet"
      #define  E4_RESULT_IDE       "Identische Schlssel in eindeutigem Subindex"
      #define  E4_RESULT_INT       "Interner Puffer fr Report zu klein"
      #define  E4_RESULT_LCF       "Datei nicht gefunden:"
      #define  E4_RESULT_LOS       "Nicht zugeordneter Block in Indexdatei"
      #define  E4_RESULT_REC       "Satznummern fr gleiche Schlssel nicht aufeinanderfolgend"
      #define  E4_RELATE_EAI       "RELATE4->error_action ist ungltig"
      #define  E4_RELATE_MEN       "RELATE4->master ist Null"
      #define  E4_RELATE_RCS       "relate4create_slave() - Typ des Master-Ausdrucks nicht kompatibel zum Subindex-Typ"
      #define  E4_RELATE_REL       "relate4match_len() erfordert Zeichenkette als Argument"
      #define  E4_RESULT_TAG       "Subindex ist veraltet"
      #define  E4_RESULT_THE       "Subindex ist nicht in Ordnung"
      #define  E4_RESULT_UNE       "Unerwartete L„nge bei Schlsselauswertung"
      #define  E4_RESULT_WAS       "Ungenutzter Platz in der Memo Datei"
      #define  E4_PARM_OPT         "Initialisierungswerte fr Optimierungs-Schalter in CODE4 zu klein"
      #define  E4_PARM_REL         "relate4create_slave() mit Datendatei aufgerufen, die bereits in der Relation verwendet wird"
      #define  E4_PARM_REP         "Versuchtes šberschreiben einer Nur-Lese-Datei"
      #define  E4_PARM_SIZ         "Versuchte L„ngen„nderung einer Nur-Lese-Datei"
      #define  E4_PARM_TAA         "i4add_tag() - Subindex existiert bereits in der Indexdatei"
      #define  E4_PARM_WRT         "Versuchtes Schreiben in eine Nur-Lese-Datei"
      #define  E4_TYPE_SUB         "Ausdruck"
      #define  E4_TYPE_UFT         "Unbekannter Feld-Typ"
      #define  E4_UNIQUE           "Subindex wird angelegt"

      /* e4severe() Meldungen*/
      #define  E4_BM4_IEL          "bitmap4seek() - ungltige Ausdruck-L„nge"  /*!!!GERMAN*/
      #define  E4_BM4_IM           "bitmap4combine_leafs() - inkompatible Bitmaps" /*!!!GERMAN*/
      #define  E4_CONST_EIN        "const4get() - Incongruente Ausdrcke"       /*!!!GERMAN*/
      #define  E4_DATA_ILL         "Nichtzul„ssige Felddaten"
      #define  E4_DATA_COR         "Fehlerhafte Datendatei."
      #define  E4_DATA_DEL         "d4delete() - ungltiges L”sch-Flag entdeckt"  /*!!!GERMAN*/
      #define  E4_DATA_DELD        "d4deleted() -ungltiges L”sch-Flag entdeckt" /*!!!GERMAN*/
      #define  E4_DATA_RECALL      "d4recall() - ungltiges L”sch-Flag"           /*!!!GERMAN*/
      #define  E4_DATA_MEM         "Memo Datei fr 'Append' wurde nicht angelegt"
      #define  E4_DATA_UPDATE      "Satznummern w„hrend Update nicht verfgbar"    /*!!!GERMAN*/
      #define  E4_EXPR_DELETED     "e4deleted() - ungltiger Wert bei deleted-Flag im Satzpuffer" /*!!!GERMAN*/
      #define  E4_INFO_ACC         "Zugriff auf NULL-Block"                 /*!!!GERMAN*/
      #define  E4_INFO_ALR         "Wurde bereits initialisiert"
      #define  E4_INFO_BDC         "Fehlerhafte Clipper NTX-Indexdatei"
      #define  E4_INFO_BDI         "Fehlerhafte FoxPro IDX-Indexdatei"
      #define  E4_INFO_BLO         "Blockgr”áe betr„gt weniger als 1024 Byte"
      #define  E4_INFO_BLS         "Eine minimale Blockgr”áe von 1024 Byte wird ben”tigt"
      #define  E4_INFO_BMC         "Blockspeicher fehlerhaft"
      #define  E4_INFO_C4C         "'CODE4.mem_size_block' hat einen unzul„ssigen Wert"
      #define  E4_INFO_COD         "Interner Fehler in CodeBase"
      #define  E4_INFO_CIF         "Indexdatei ist besch„digt"
      #define  E4_INFO_CIB         "Indexblock ist besch„digt"
      #define  E4_INFO_CIS         "Indexblockstruktur ist besch„digt"
      #define  E4_INFO_CMF         "Memodatei ist besch„digt"
      #define  E4_INFO_CRL         "Verkettete Liste ist besch„digt"
      #define  E4_INFO_DAO         "Datenbank-Datei bereits ge”ffnet"
      #define  E4_INFO_DEL         "zu l”schender Block ist nicht leer"
      #define  E4_INFO_DUP         "Doppelter Alias-Wert"
      #define  E4_INFO_EPV         "Erwarteter Wert wurde nicht gebildet"
      #define  E4_INFO_EVA         "Indexausdruck wird ausgewertet"
      #define  E4_INFO_EXK         "Schlsselausdruck berschreitet die H”chstl„nge von 102 Zeichen"
      #define  E4_INFO_EXP         "Ausdruck ist zu lang"
      #define  E4_INFO_FAI         "Fehler in logischem Feld-Index"
      #define  E4_INFO_FOR         "fr Subindex"
      #define  E4_INFO_ILF         "nichtzul„ssiger Bezug auf Dateiblock"
      #define  E4_INFO_ILL         "nichtzul„ssige Konfiguration beim Locken"
      #define  E4_INFO_ILP         "nichtzul„ssige Position"
      #define  E4_INFO_IMS         "Ungltige Memo-Gr”áe in memo4file_write()"
      #define  E4_INFO_INT         "interner Fehler in t4remove_current(). Indexdatei besch„digt"
      #define  E4_INFO_INV         "Ungltiger Schlsseltyp in der Indexdatei"
      #define  E4_INFO_IAO         "Indexdatei bereits ge”ffnet"
      #define  E4_INFO_IVE         "Ungltige Fehler-Aktion"
      #define  E4_INFO_IVT         "Ungltiger Typ"
      #define  E4_INFO_LIN         "Link befindet sich nicht in der Liste"
      #define  E4_INFO_NKE         "n_keys ungltig"
      #define  E4_INFO_STR         "Funktionsaufruf mit ungltigem Strukturzeiger-Argument"
      #define  E4_INFO_TAO         "Indexdatei bereits ge”ffnet"
      #define  E4_INFO_UNE         "Unerwarteter leerer Block"
      #define  E4_INFO_UNI         "nicht initialisierte Werte in opt4add_block"
      #define  E4_INFO_UNT         "Unerwartete Indexsperren in der Indexdatei"
      #define  E4_INFO_WRO         "Falsche Anzahl von Links in verketteter Liste"
      #define  E4_INFO_WT4         "Falscher TAG4 Typ"
      #define  E4_MEMORY_ERR       "Speicherverwaltungs-Fehler"
      #define  E4_MEMORY_OOR       "Ungltige Speichergr”áe in const4mem_alloc()" /*!!!GERMAN*/
      #define  E4_MEMORY_SAV       "l4lock_save(), Information zum Sperren wird gespeichert"
      #define  E4_MEMORY_YPO       "mem4pop_pointer() mit S4DEBUG-Schalter"
      #define  E4_MEMORY_YPU       "mem4push_pointer() mit S4DEBUG-Schalter"
      #define  E4_OPEN_NOD         "i4open(): keine Datenbank angegeben"
      #define  E4_PARM_FLA         "Wert des Flags sollte 'r4descending' sein"
      #define  E4_PARM_FOR         "Fr die Version S4NDX sollten 'filter' und 'descending' gleich 0 sein"
      #define  E4_PARM_H4R         "file4read() mit negativer Position"
      #define  E4_PARM_ILP         "unzul„ssige Position"
      #define  E4_PARM_IND         "Indexdatei bereits ge”ffnet"
      #define  E4_PARM_INV         "Ungltige Position beim Sperren"
      #define  E4_PARM_NFD         "relate4sort(): nicht freigegebene Datenliste"
      #define  E4_PARM_NUL         "Null-Zeiger auf Datendatei"
      #define  E4_PARM_NSD         "Null-String entdeckt"
      #define  E4_PARM_OSD         "šberlappende Strings entdeckt"
      #define  E4_PARM_REC         "Datensatznummern nicht aufeinanderfolgend"
      #define  E4_PARM_TAG         "Subindex-Ausdruck fehlt"
      #define  E4_PARM_TOO         "Zu viele Subindizes"
      #define  E4_PARM_UNI         "Das unique-Flag hat einen unzul„ssigen Wert"
      #define  E4_PARM_ZER         "Nullparameter"
      #define  E4_RESULT_CLI       "Schlsselkonvertierung fr CLIPPER gescheitert"
      #define  E4_RESULT_CMP       "mem4check_pointer() - falscher Zeigerwert entdeckt"
      #define  E4_RESULT_COM       "fehlerhafter Speicherinhalt entdeckt"
      #define  E4_RESULT_COR       "fehlerhafte Indexdatei wird geprft"
      #define  E4_RESULT_D4A       "d4append_start() wurde nicht abgeschlossen"
      #define  E4_RESULT_D4I       "d4init() wurde nicht aufgerufen."
      #define  E4_RESULT_EXI       "Warteposition beim Sperren wird abgebrochen"
      #define  E4_RESULT_FRE       "Freier Wert nicht gefunden"
      #define  E4_RESULT_INC       "Falsche Reihenfolge beim Sperren der Indexdatei"
      #define  E4_RESULT_INQ       "bei QuickSort."
      #define  E4_RESULT_LOC       "Indexsperre k”nnte einen 'deadlock' bewirken"
      #define  E4_RESULT_LCO       "šberlappung beim Sperren entdeckt"
      #define  E4_RESULT_MEM       "dynamische Speichervariablen nicht freigegeben"
      #define  E4_RESULT_REM       "Nicht vorgenommene Sperre wird entfernt"
      #define  E4_RESULT_S4L       "S4LOCK_CHECK kann nicht mit 'mem4reset' verwendet werden"
      #define  E4_RESULT_STC       "Die Stack-Variable _stklen ist zu klein"
      #define  E4_RESULT_TOO       "Zuviele Zeiger vorhanden"

      #ifdef S4WINDOWS
         #define E4_REPORT_BIT        "Kann Bitmap nicht erstellen"
         #define E4_REPORT_COD        "CodeReporter"
         #define E4_REPORT_CRE        "Kann Ausgabefenster nicht erstellen"
         #define E4_REPORT_DIS        "Kein Display-Context verfgbar"
         #define E4_REPORT_FON        "Fehler bei Font-Erstellung"
         #define E4_REPORT_PRI        "Drucke"

         #define E4_RESULT_CRE        "Anlegen eines Drucker-Bildschirm-Kontextes"
         #define E4_RESULT_DEL        "L”schen eines Bildschirm-Kontextes"
         #define E4_RESULT_END        "Ein Dokument wird beendet"
         #define E4_RESULT_G4D        "G4DISPLAY-Puffer fehlt"
         #define E4_RESULT_G4E        "G4EDIT-Puffer fehlt"
         #define E4_RESULT_G4L        "G4LIST-Puffer fehlt"
         #define E4_RESULT_ON         "Neues Formular"
         #define E4_RESULT_SET        "bei Font-Definition"
         #define E4_RESULT_STA        "Drucken eines Dokuments beginnt"
         #define E4_MESSAG_EXI        "Abbruch der Anwendung"
         #define E4_MESSAG_CAN        "Abbruch"
         #define E4_MESSAG_OK         "OK"
         #define E4_MESSAG_PAC        "Sind Sie sicher, daá Sie packen m”chten?"
         #define E4_MESSAG_REC        "SATZNUMMER"
         #define E4_MESSAG_REI        "Sind Sie sicher, daá Sie neu indizieren m”chten?"
         #define E4_PARM_ADD          "L„nge der Auswahl muá > 0 sein"
         #define E4_PARM_SA           "Ungltiger G4DISPLAY-Zeiger"
         #define E4_PARM_VIS          "WS_VISIBLE-Attribut nicht gesetzt"
         #define E4_PARM_WIN          "Ungltige Fensterabmessungen"
         #define E4_RESULT_ADD        "String kann nicht hinzugefgt werden"
         #define E4_RESULT_BUF        "G4LIST-Puffer nicht groá genug"
         #define E4_RESULT_CDC        "CreateCompatibleDC() gescheitert"
         #define E4_RESULT_ENW        "EnableWindow() gescheitert"
         #define E4_RESULT_GDC        "GetDC() gescheitert"
         #define E4_RESULT_GTM        "GetTextMetrics() gescheitert"
         #define E4_RESULT_ID         "Ungltige ID-Nummer"
         #define E4_RESULT_IDB        "Keine G4BUTTON-ID-Nummer"
         #define E4_RESULT_IDC        "Keine G4COMBO-ID-Nummer"
         #define E4_RESULT_IDD        "Keine G4DISPLAY-ID Nummer"
         #define E4_RESULT_IDL        "Keine G4LIST-ID-Nummer"
         #define E4_RESULT_IDT        "Keine G4EDIT-ID-Nummer"
         #define E4_RESULT_PIC        "Falsches Eingabeformat"
         #define E4_RESULT_POS        "Falsche Position des Carets"
         #define E4_RESULT_RDC        "ReleaseDC() gescheitert"
         #define E4_RESULT_RES        "FindResource() gescheitert"
         #define E4_RESULT_TA1        "WM_TABSTOP-Attribut nicht gesetzt"
         #define E4_RESULT_TA2        "Kontroll-Element kann kein WM_TABSTOP-Attribut haben"
         #define E4_RESULT_WIN        "CreateWindow() gescheitert"
      #endif
#endif   /* ifdef S4GERMAN  */

#ifdef S4FRENCH
      #define  E4_CLOSE            "Unknown Name"
      #define  E4_CREATE_FIL       "File Name:"
      #define  E4_CREATE_TEM       "Temporary Working File"
      #define  E4_CREATE_TOO       "Too Many Fields"
      #define  E4_ERROR            "Error"
      #define  E4_ERROR_NUM        "\r\n\r\nError Number"
      #define  E4_ERROR_SEV        "\r\n\r\nSevere Error Number"
      #define  E4_ERROR_BAS        "CODEBASIC ERROR"
      #define  E4_ERROR_COD        "CODEBASE ERROR"
      #define  E4_ERROR_CDS        "CODEBASE SEVERE ERROR"
      #define  E4_ERROR_ENT        "\r\nPress Enter ..."
      #define  E4_ERROR_KEY        "\r\nPress a key ..."
      #define  E4_EXPR_OM          "expression source length > max allowable chars for index"
      #define  E4_INDEX_COR        "Corrupted Free Block Chain in Index File"
      #define  E4_INDEX_EXP        "Expected '.CDX' file is of the '.IDX' type"
      #define  E4_INDEX_FIL        "Filter should be Logical"
      #define  E4_INFO_AME         "A memo block is used twice."
      #define  E4_INFO_BAC         "Backwards skip attempted without calling relate4skip_enable()"
      #define  E4_INFO_CAL         "Call to d4reccount() in r4reindex_blocks_alloc() failed"
      #define  E4_INFO_CLO         "Error closing group file in i4open()"
      #define  E4_INFO_DAT         "Database is empty."
      #define  E4_INFO_EMF         "Expected memo field non-existant in f4memo_read_low()"
      #define  E4_INFO_IFT         "f4assign_field() - invalid field type"
      #define  E4_INFO_INC         "Incorrect Record Number in tag:"
      #define  E4_INFO_KEY         "Error in Key Evaluation"
      #define  E4_INFO_LOC         "Error locking in i4open()"
      #define  E4_INFO_REA         "Error reading group file in i4open()"
      #define  E4_INFO_REC         "Record is missing."
      #define  E4_INFO_REL         "relate4top() must be called first"
      #define  E4_INFO_REP         "Repeated Record Number in tag:"
      #define  E4_INFO_SET         "Set CODE4.auto_open to 'off'"
      #define  E4_INFO_SKL         "CODE4->stored_key_len too small"
      #define  E4_INFO_SKI         "Skip must be made from valid record number"
      #define  E4_INFO_TAG         "Tag is corrupted"
      #define  E4_LOCK_LOC         "Attempt to lock file that is read-only"
      #define  E4_LOCK_UNL         "Attempt to unlock file that is read-only"
      #define  E4_MEMORY_B         "Block Size too Large for Indexing"
      #define  E4_MEMORY_S         "Sorting"
      #define  E4_NUM_PARMS        "Function"
      #define  E4_OPT_INV          "Invalid mode"
      #define  E4_REPORT_FILE      "This file is not a CodeReporter report file."
      #define  E4_REPORT_ONE       "Only one report can be loaded at a time"
      #define  E4_REPORT_DFILE     "Unable to Open Data File"
      #define  E4_REPORT_GRO       "Group not Found while Retrieving Report"
      #define  E4_REPORT_IFILE     "Index File Not Found"
      #define  E4_REPORT_ASTYLE    "Unable to Allocate Memory For Style"
      #define  E4_REPORT_AGROUP    "Unable to Allocate Memory For Group"
      #define  E4_REPORT_RGROUP    "Unable to Retrieve Group Information"
      #define  E4_REPORT_ATOTAL    "Unable to Allocate Memory for Total"
      #define  E4_REPORT_ATEXT     "Unable to Allocate Memory for Text Object"
          #define  E4_REPORT_HINST     "'hInst' member of CODE4 is not Initialized"
          #define  E4_REPORT_HWND      "'hWndParent' member of REPORT4 is not Initialized"
      #define  E4_REPORT_RTEXT     "Unable to Retrieve Text Object Information"
      #define  E4_REPORT_RCALC     "Unable to Retrieve Calculation Information"
      #define  E4_REPORT_RTOTAL    "Unable to Retrieve Total Information"
      #define  E4_REPORT_TRG       "Unable to Locate Reset Group"
      #define  E4_REPORT_ROBJS     "Unable to Retrieve Objects Information"
      #define  E4_RESULT_CII       "Clipper incompatible Index File detected."
      #define  E4_RESULT_D4F       "d4flush_record() did not work."
      #define  E4_RESULT_DAT       "Database not found"
      #define  E4_RESULT_EXP       "Expecting logical result."
      #define  E4_RESULT_IDE       "Identical keys in unique key tag:"
      #define  E4_RESULT_INT       "Internal buffer too small while retrieving report" 
      #define  E4_RESULT_LCF       "Could not locate File:"
      #define  E4_RESULT_LOS       "Lost Block in Index File"
      #define  E4_RESULT_REC       "Record numbers out of order for equal keys:"
      #define  E4_RELATE_EAI       "RELATE4->error_action invalid"
      #define  E4_RELATE_MEN       "RELATE4->master expected is null"
      #define  E4_RELATE_RCS       "relate4create_slave() - master expression type does not match tag type"
      #define  E4_RELATE_REL       "relate4match_len() can only be called with a character expression"
      #define  E4_RESULT_TAG       "Tag is out of date:"
      #define  E4_RESULT_THE       "The tag is out of order:"
      #define  E4_RESULT_UNE       "Unexpected key evaluation length:"
      #define  E4_RESULT_WAS       "Wasted SPace in Memo File"
      #define  E4_PARM_MUS         "relate4top() must be called first"
      #define  E4_PARM_OPT         "CODE4 optimization initialization values too small" 
      #define  E4_PARM_REL         "Called relate4create_slave() with a database that already exists in another part of the relation" 
      #define  E4_PARM_REP         "Attempt to replace to a read-only file" 
      #define  E4_PARM_SIZ         "Attempt to change file length of a read-only file"
      #define  E4_PARM_TAA         "i4add_tag() - an input tag already exists in the index file"
      #define  E4_PARM_WRT         "Attempt to write to a read-only file" 
      #define  E4_TYPE_SUB         "Expression:"
      #define  E4_TYPE_UFT         "Unknown field type"
      #define  E4_UNIQUE           "Creating Tag"

      /* e4severe() messages */
      #define  E4_BM4_IEL          "bitmap4seek() - invalid expression length"
      #define  E4_BM4_IM           "bitmap4combine_leafs() - incompatible maps"
      #define  E4_CONST_EIN        "const4get() - expression incongruity"
      #define  E4_DATA_ILL         "Illegal field data."
      #define  E4_DATA_COR         "Corrupt Database File."
      #define  E4_DATA_DEL         "d4delete() - invalid deletion flag detected"
      #define  E4_DATA_DELD        "d4deleted() - invalid deletion flag detected"
      #define  E4_DATA_RECALL      "d4recall() - invalid deletion flag"
      #define  E4_DATA_MEM         "Memo File not Open for Append"
      #define  E4_DATA_UPDATE      "Number records unknown at update time"
      #define  E4_EXPR_DELETED     "e4deleted() - invalid value detected in record buffer's deleted flag"
      #define  E4_INFO_ACC         "Accessing NULL Block"
      #define  E4_INFO_ALR         "Already initialized"
      #define  E4_INFO_BDC         "Corrupt NTX Clipper index file"
      #define  E4_INFO_BDI         "Corrupt IDX FoxPro index file"
      #define  E4_INFO_BLO         "Block size is less than 1024 bytes"
      #define  E4_INFO_BLS         "This is unsupported since dBASE IV compatibility requires minimum blocks sizes of 1024 bytes"
      #define  E4_INFO_BMC         "Block/memory corruption"
      #define  E4_INFO_C4C         "'CODE4.mem_size_block' is an illegal value."
      #define  E4_INFO_COD         "CodeBase internal error"
      #define  E4_INFO_CIF         "Corrupt index file"
      #define  E4_INFO_CIB         "Corrupt index block"
      #define  E4_INFO_CIS         "Corrupt index structure"
      #define  E4_INFO_CMF         "Corrupt memo file"
      #define  E4_INFO_CRL         "Corrupted Linked List"
      #define  E4_INFO_DAO         "Database file already open"
      #define  E4_INFO_DEL         "Deleted block not empty"
      #define  E4_INFO_DUP         "Duplicate Alias"
      #define  E4_INFO_EPV         "Expected value not built"
      #define  E4_INFO_EVA         "Evaluating Tag Expression"
      #define  E4_INFO_EXK         "Expression key is larger than maximum 102 characters"
      #define  E4_INFO_EXP         "Expression too large"
      #define  E4_INFO_FAI         "Failure in logical field index"
      #define  E4_INFO_FOR         "for tag"
      #define  E4_INFO_ILF         "Illegal file block reference"
      #define  E4_INFO_ILL         "Illegal locking configuration"
      #define  E4_INFO_ILP         "Illegal position"
      #define  E4_INFO_IMS         "Invalid memo size in memo4file_write()"
      #define  E4_INFO_INT         "Internal t4remove_current() failure--index file corrupt"
      #define  E4_INFO_INV         "Invalid key type in index file"
      #define  E4_INFO_IVE         "Invalid error action"
      #define  E4_INFO_IVT         "Invalid type"
      #define  E4_INFO_LIN         "Link not on list"
      #define  E4_INFO_NKE         "n_keys invalid"
      #define  E4_INFO_STR         "Function was Passed an Invalid Structure Pointer"
      #define  E4_INFO_UNE         "Unexpected empty block"
      #define  E4_INFO_UNI         "Un-initialized values in opt4add_block"
      #define  E4_INFO_UNT         "Unexpected tag locks in index file"
      #define  E4_INFO_WRO         "Wrong number of links in linked list"
      #define  E4_INFO_WT4         "Wrong TAG4 Type"
      #define  E4_MEMORY_ERR       "Memory Error"
      #define  E4_MEMORY_OOR       "Memory out of range in const4mem_alloc()"
      #define  E4_MEMORY_SAV       "l4lock_save(), Saving Lock Information"
      #define  E4_MEMORY_YPO       "mem4pop_pointer() with S4DEBUG switch."
      #define  E4_MEMORY_YPU       "mem4push_pointer() with S4DEBUG switch."
      #define  E4_OPEN_NOD         "i4open(): No database specified"
      #define  E4_PARM_FLA         "Flag should be 'r4descending'."
      #define  E4_PARM_FOR         "For S4NDX version, filter and descending should be 0"
      #define  E4_PARM_H4R         "file4read() with negative position"
      #define  E4_PARM_ILP         "Illegal position"
      #define  E4_PARM_IND         "Index file already open"
      #define  E4_PARM_NFD         "relate4sort(): Non-freed data list"
      #define  E4_PARM_INV         "Invalid lock position"
      #define  E4_PARM_NSD         "Null String Detected"
      #define  E4_PARM_OSD         "Overlapping Strings Detected"
      #define  E4_PARM_NUL         "Null Data File Pointer"
      #define  E4_PARM_REC         "Record Numbers are not Consecutive"
      #define  E4_PARM_TAG         "Tag expression is missing."
      #define  E4_PARM_TOO         "Too many tags"
      #define  E4_PARM_UNI         "Unique flag data is an unrecognized value."
      #define  E4_PARM_ZER         "Zero parameter"
      #define  E4_RESULT_CLI       "Clipper Key Conversion failed"
      #define  E4_RESULT_CMP    "mem4check_pointer() - corrupt memory pointer encountered"
      #define  E4_RESULT_COM       "Corrupted Memory Detected"
      #define  E4_RESULT_COR       "Corrupted Index File in Check"
      #define  E4_RESULT_D4A       "d4append_start() was not completed."
      #define  E4_RESULT_D4I       "d4init() has not been called."
      #define  E4_RESULT_EXI       "Exiting from within lock wait"
      #define  E4_RESULT_FRE       "Free value not found"
      #define  E4_RESULT_INC       "Incorrect index file locking order."
      #define  E4_RESULT_INQ       "In quick sort."
      #define  E4_RESULT_LOC       "Locking index could create deadlock."
      #define  E4_RESULT_LCO       "Locking Overlap Detected"
      #define  E4_RESULT_MEM       "Memory items not freed"
      #define  E4_RESULT_REM       "Removing Lock which was never placed."
      #define  E4_RESULT_S4L       "S4LOCK_CHECK cannot be used with 'mem4reset'"
      #define  E4_RESULT_STC       "Stack length variable _stklen below minimum."
      #define  E4_RESULT_TOO       "Too many pointers"

      #ifdef S4WINDOWS
         #define E4_REPORT_BIT        "Could not create a bitmap"
         #define E4_REPORT_COD        "CodeReporter"
         #define E4_REPORT_CRE        "Could not create output window"
         #define E4_REPORT_DIS        "Could not get a display context"
         #define E4_REPORT_FON        "Error Creating Font"
         #define E4_REPORT_PRI        "Printing"
   
         #define E4_RESULT_CRE        "Creating Printer Display Context"
         #define E4_RESULT_DEL        "Deleting Display Context"
         #define E4_RESULT_END        "Ending Document"
         #define E4_RESULT_G4D        "G4DISPLAY buffer missing"
         #define E4_RESULT_G4E        "G4EDIT buffer missing"
         #define E4_RESULT_G4L        "G4LIST buffer missing"
         #define E4_RESULT_ON         "On New Frame"
         #define E4_RESULT_SET        "Setting Font"
         #define E4_RESULT_STA        "Starting Print Document"
         #define E4_MESSAG_EXI        "EXITING APPLICATION"
         #define E4_MESSAG_CAN        "Cancel"
         #define E4_MESSAG_OK         "OK"
         #define E4_MESSAG_PAC        "Are you sure you want to pack ?"
         #define E4_MESSAG_REC        "RECORD NUMBER"
         #define E4_MESSAG_REI        "Are you sure you want to reindex ?"
         #define E4_PARM_ADD          "Choice length must be > 0"
         #define E4_PARM_SA           "Invalid G4DISPLAY Pointer"
         #define E4_PARM_VIS          "WS_VISIBLE Style not Present"
         #define E4_PARM_WIN          "Invalid Window Dimensions"
         #define E4_RESULT_ADD        "Add String Failed"
         #define E4_RESULT_BUF        "G4LIST Buffer Not Big Enough"
         #define E4_RESULT_CDC        "CreateCompatibleDC() Failed"
         #define E4_RESULT_ENW        "EnableWindow() Failed"
         #define E4_RESULT_GDC        "GetDC() Failed"
         #define E4_RESULT_GTM        "GetTextMetrics Failed"
         #define E4_RESULT_ID         "Invalid Id Number"
         #define E4_RESULT_IDB        "Not A G4BUTTON Id Number"
         #define E4_RESULT_IDC        "Not A G4COMBO Id Number"
         #define E4_RESULT_IDD        "Not A G4DISPLAY Id Number"
         #define E4_RESULT_IDL        "Not A G4LIST Id Number"
         #define E4_RESULT_IDT        "Not A G4EDIT Id Number"
         #define E4_RESULT_PIC        "Bad Picture"
         #define E4_RESULT_POS        "Bad Caret Position"
         #define E4_RESULT_RDC        "ReleaseDC() Failed"
         #define E4_RESULT_RES        "FindResource() Failed"
         #define E4_RESULT_TA1        "No WM_TABSTOP Style Present"
         #define E4_RESULT_TA2        "Control Cannot Have The WM_TABSTOP Style"
         #define E4_RESULT_WIN        "CreateWindow() Failed"
      #endif

      #define  E4A_GRA   0x85
      #define  E4A_CIR   0x83
      #define  E4A_CI2   0x86
      #define  E4A_TRE   0x84
      #define  E4A_EGU   0xA0
      #define  E4C_CED   0x87
      #define  E4E_EGU   0x82
      #define  E4E_GRA   0x8A
      #define  E4E_CIR   0x88
      #define  E4E_TRE   0x89
      #define  E4I_TRE   0x8B
      #define  E4I_CIR   0x8C
      #define  E4I_EGU   0xA1
      #define  E4I_GRA   0x8D
      #define  E4N_ACC   0xA4
      #define  E4O_CIR   0x93
      #define  E4O_TRE   0x94
      #define  E4O_GRA   0x95
      #define  E4O_EGU   0xA2
      #define  E4U_CIR   0x96
      #define  E4U_GRA   0x97
      #define  E4U_TRE   0x81
      #define  E4U_EGU   0xA3
      #define  E4Y_TRE   0x98
      #define  E4CM_CED  0x80
      #define  E4AM_TRE  0x8E
      #define  E4AM_CIR  0x8F
      #define  E4EM_EGU  0x90
      #define  E4OM_TRE  0x89
      #define  E4UM_TRE  0x9A

#endif   /* ifdef S4FRENCH  */

#ifdef S4SWEDISH
   /* e4() messages */
   #define  E4_CLOSE         "Unknown Name"
   #define  E4_CREATE_FIL    "File Name:"
   #define  E4_CREATE_TEM    "Temporary Working File"
   #define  E4_CREATE_TOO    "Too Many Fields"
   #define  E4_ERROR         "Error"
   #define  E4_ERROR_NUM     "\r\n\r\nError Number"
   #define  E4_ERROR_SEV     "\r\n\r\nSevere Error Number"
   #define  E4_ERROR_BAS     "CODEBASIC ERROR"
   #define  E4_ERROR_COD     "CODEBASE ERROR"
   #define  E4_ERROR_CDS     "CODEBASE SEVERE ERROR"
   #define  E4_ERROR_ENT     "\r\nPress Enter ..."
   #define  E4_ERROR_KEY     "\r\nPress a key ..."
   #define  E4_EXPR_OM       "expression source length > max allowable chars for index"
   #define  E4_INDEX_COR     "Corrupted Free Block Chain in Index File"
   #define  E4_INDEX_EXP     "Expected '.CDX' file is of the '.IDX' type"
   #define  E4_INDEX_FIL     "Filter should be Logical"
   #define  E4_INFO_AME      "A memo block is used twice."
   #define  E4_INFO_BAC      "Backwards skip attempted without calling relate4skip_enable()"
   #define  E4_INFO_CAL      "Call to d4reccount() in r4reindex_blocks_alloc() failed"
   #define  E4_INFO_CLO      "Error closing group file in i4open()"
   #define  E4_INFO_DAT      "Database is empty."
   #define  E4_INFO_EMF      "Expected memo field non-existant in f4memo_read_low()"
   #define  E4_INFO_IFT      "f4assign_field() - invalid field type"
   #define  E4_INFO_INC      "Incorrect Record Number in tag:"
   #define  E4_INFO_KEY      "Error in Key Evaluation"
   #define  E4_INFO_LOC      "Error locking in i4open()"
   #define  E4_INFO_REA      "Error reading group file in i4open()"
   #define  E4_INFO_REC      "Record is missing."
   #define  E4_INFO_REL      "relate4top() must be called first"
   #define  E4_INFO_REP      "Repeated Record Number in tag:"
   #define  E4_INFO_SET      "Set CODE4.auto_open to 'off'"
   #define  E4_INFO_SKL      "CODE4->stored_key_len too small"
   #define  E4_INFO_SKI      "Skip must be made from valid record number"
   #define  E4_INFO_TAG      "Tag is corrupted"
   #define  E4_LOCK_LOC      "Attempt to lock file that is read-only"
   #define  E4_LOCK_UNL      "Attempt to unlock file that is read-only"
   #define  E4_MEMORY_B      "Block Size too Large for Indexing"
   #define  E4_MEMORY_S      "Sorting"
   #define  E4_NUM_PARMS     "Function"
   #define  E4_OPT_INV       "Invalid mode"
   #define  E4_RESULT_CII    "Clipper incompatible Index File detected."
   #define  E4_RESULT_D4F    "d4flush_record() did not work."
   #define  E4_RELATE_EAI    "RELATE4->error_action invalid"
   #define  E4_RELATE_MEN    "RELATE4->master expected is null"
   #define  E4_RELATE_RCS    "relate4create_slave() - master expression type does not match tag type"
   #define  E4_RELATE_REL    "relate4match_len() can only be called with a character expression"
   #define  E4_REPORT_FILE   "This file is not a CodeReporter report file."
   #define  E4_REPORT_ONE    "Only one report can be loaded at a time"
   #define  E4_REPORT_DFILE  "Unable to Open Data File"
   #define  E4_REPORT_GRO    "Group Not Found while Retrieving Report"
   #define  E4_REPORT_IFILE  "Index File Not Found"
   #define  E4_REPORT_ASTYLE "Unable to Allocate Memory For Style"
   #define  E4_REPORT_AGROUP "Unable to Allocate Memory For Group"
   #define  E4_REPORT_RGROUP "Unable to Retrieve Group Information"
   #define  E4_REPORT_ATOTAL "Unable to Allocate Memory for Total"
   #define  E4_REPORT_ATEXT  "Unable to Allocate Memory for Text Object"
   #define  E4_REPORT_HINST  "'hInst' member of CODE4 is not Initialized"
   #define  E4_REPORT_HWND   "'hWndParent' member of REPORT4 is not Initialized"
   #define  E4_REPORT_RTEXT  "Unable to Retrieve Text Object Information"
   #define  E4_REPORT_RCALC  "Unable to Retrieve Calculation Information"
   #define  E4_REPORT_RTOTAL "Unable to Retrieve Total Information"
   #define  E4_REPORT_TRG    "Unable to Locate Reset Group"
   #define  E4_REPORT_ROBJS  "Unable to Retrieve Objects Information"
   #define  E4_RESULT_DAT    "Database not found"
   #define  E4_RESULT_EXP    "Expecting logical result."
   #define  E4_RESULT_IDE    "Identical keys in unique key tag:"
   #define  E4_RESULT_INT    "Internal buffer too small while retrieving report" 
   #define  E4_RESULT_LCF    "Could not locate File:"
   #define  E4_RESULT_LOS    "Lost Block in Index File"
   #define  E4_RESULT_REC    "Record numbers out of order for equal keys:"
   #define  E4_RESULT_TAG    "Tag is out of date:"
   #define  E4_RESULT_THE    "The tag is out of order:"
   #define  E4_RESULT_UNE    "Unexpected key evaluation length:"
   #define  E4_RESULT_WAS    "Wasted Space in Memo File"
   #define  E4_PARM_MUS      "relate4top() must be called first"
   #define  E4_PARM_OPT      "CODE4 optimization initialization values too small" 
   #define  E4_PARM_REL      "Called relate4create_slave() with a database that already exists in another part of the relation" 
   #define  E4_PARM_REP      "Attempt to replace to a read-only file" 
   #define  E4_PARM_SIZ      "Attempt to change file length of a read-only file"
   #define  E4_TYPE_SUB      "Expression:"
   #define  E4_PARM_TAA      "i4add_tag() - an input tag already exists in the index file"
   #define  E4_PARM_WRT      "Attempt to write to a read-only file" 
   #define  E4_TYPE_UFT      "Unknown field type"
   #define  E4_UNIQUE        "Creating Tag"

   /* e4severe() messages */
   #define  E4_BM4_IEL       "bitmap4seek() - invalid expression length"
   #define  E4_BM4_IM        "bitmap4combine_leafs() - incompatible maps"
   #define  E4_CONST_EIN     "const4get() - expression incongruity"
   #define  E4_DATA_ILL      "Illegal field data."
   #define  E4_DATA_COR      "Corrupt Database File."
   #define  E4_DATA_DEL      "d4delete() - invalid deletion flag detected"
   #define  E4_DATA_DELD     "d4deleted() - invalid deletion flag detected"
   #define  E4_DATA_MEM      "Memo File not Open for Append"
   #define  E4_DATA_UPDATE   "Number records unknown at update time"
   #define  E4_DATA_RECALL   "d4recall() - invalid deletion flag"
   #define  E4_EXPR_DELETED  "e4deleted() - invalid value detected in record buffer's deleted flag"
   #define  E4_INFO_ACC      "Accessing NULL Block"
   #define  E4_INFO_ALR      "Already initialized"
   #define  E4_INFO_BDC      "Corrupt NTX Clipper index file"
   #define  E4_INFO_BDI      "Corrupt IDX FoxPro index file"
   #define  E4_INFO_BLO      "Block size is less than 1024 bytes"
   #define  E4_INFO_BLS      "This is unsupported since dBASE IV compatibility requires minimum blocks sizes of 1024 bytes"
   #define  E4_INFO_BMC      "Block/memory corruption"
   #define  E4_INFO_C4C      "'CODE4.mem_size_block' is an illegal value."
   #define  E4_INFO_COD      "CodeBase internal error"
   #define  E4_INFO_CIF      "Corrupt index file"
   #define  E4_INFO_CIB      "Corrupt index block"
   #define  E4_INFO_CIS      "Corrupt index structure"
   #define  E4_INFO_CMF      "Corrupt memo file"
   #define  E4_INFO_CRL      "Corrupted Linked List"
   #define  E4_INFO_DAO      "Database file already open"
   #define  E4_INFO_DEL      "Deleted block not empty"
   #define  E4_INFO_DUP      "Duplicate Alias"
   #define  E4_INFO_EPV      "Expected value not built"
   #define  E4_INFO_EVA      "Evaluating Tag Expression"
   #define  E4_INFO_EXK      "Expression key is larger than maximum 102 characters"
   #define  E4_INFO_EXP      "Expression too large"
   #define  E4_INFO_FAI      "Failure in logical field index"
   #define  E4_INFO_FOR      "For tag"
   #define  E4_INFO_ILF      "Illegal file block reference"
   #define  E4_INFO_ILL      "Illegal locking configuration"
   #define  E4_INFO_ILP      "Illegal position"
   #define  E4_INFO_IMS      "Invalid memo size in memo4file_write()"
   #define  E4_INFO_INT      "Internal t4remove_current() failure--index file corrupt"
   #define  E4_INFO_INV      "Invalid key type in index file"
   #define  E4_INFO_IAO      "Index file already open"
   #define  E4_INFO_IVE      "Invalid error action"
   #define  E4_INFO_IVT      "Invalid type"
   #define  E4_INFO_LIN      "Link not on list"
   #define  E4_INFO_NKE      "n_keys invalid"
   #define  E4_INFO_STR      "Function was Passed an Invalid Structure Pointer"
   #define  E4_INFO_TAO      "Tag file already open"
   #define  E4_INFO_UNE      "Unexpected empty block"
   #define  E4_INFO_UNI      "Un-initialized values in opt4add_block"
   #define  E4_INFO_UNT      "Unexpected tag locks in index file"
   #define  E4_INFO_WRO      "Wrong number of links in linked list"
   #define  E4_INFO_WT4      "Wrong TAG4 Type"
   #define  E4_MEMORY_ERR    "Memory Error"
   #define  E4_MEMORY_OOR    "Memory out of range in const4mem_alloc()"
   #define  E4_MEMORY_SAV    "l4lock_save(), Saving Lock Information"
   #define  E4_MEMORY_YPO    "mem4pop_pointer() with S4DEBUG switch."
   #define  E4_MEMORY_YPU    "mem4push_pointer() with S4DEBUG switch."
   #define  E4_OPEN_NOD      "i4open(): No database specified"
   #define  E4_PARM_FLA      "Flag should be 'r4descending'."
   #define  E4_PARM_FOR      "For S4NDX version, filter and descending should be 0"
   #define  E4_PARM_ILP      "Illegal position"
   #define  E4_PARM_IND      "Index file already open"
   #define  E4_PARM_INV      "Invalid lock position"
   #define  E4_PARM_NFD      "relate4sort(): Non-freed data list"
   #define  E4_PARM_NSD      "Null String Detected"
   #define  E4_PARM_OSD      "Overlapping Strings Detected"
   #define  E4_PARM_NUL      "Null Data File Pointer"
   #define  E4_PARM_REC      "Record Numbers are not Consecutive"
   #define  E4_PARM_TAG      "Tag expression is missing."
   #define  E4_PARM_TOO      "Too many tags"
   #define  E4_PARM_UNI      "Unique flag data is an unrecognized value."
   #define  E4_PARM_ZER      "Zero parameter"
   #define  E4_RESULT_CLI    "Clipper Key Conversion failed"
   #define  E4_RESULT_CMP    "mem4check_pointer() - corrupt memory pointer encountered"
   #define  E4_RESULT_COM    "Corrupted Memory Detected"
   #define  E4_RESULT_COR    "Corrupted Index File in Check"
   #define  E4_RESULT_D4A    "d4append_start() was not completed."
   #define  E4_RESULT_D4I    "d4init() has not been called."
   #define  E4_RESULT_EXI    "Exiting from within lock wait"
   #define  E4_RESULT_FRE    "Free value not found"
   #define  E4_RESULT_INC    "Incorrect index file locking order."
   #define  E4_RESULT_INQ    "In quick sort."
   #define  E4_RESULT_LOC    "Locking index could create deadlock."
   #define  E4_RESULT_LCO    "Locking Overlap Detected"
   #define  E4_RESULT_MEM    "Memory items not freed"
   #define  E4_RESULT_REM    "Removing Lock which was never placed."
   #define  E4_RESULT_S4L    "S4LOCK_CHECK cannot be used with 'mem4reset'"
   #define  E4_RESULT_STC    "Stack length variable _stklen below minimum."
   #define  E4_RESULT_TOO    "Too many pointers"

   #ifdef S4WINDOWS
      #define E4_REPORT_BIT     "Could not create a bitmap"
      #define E4_REPORT_COD     "CodeReporter"
      #define E4_REPORT_CRE     "Could not create output window"
      #define E4_REPORT_DIS     "Could not get a display context"
      #define E4_REPORT_FON     "Error Creating Font"
      #define E4_REPORT_PRI     "Printing"
      #define E4_RESULT_CRE     "Creating Printer Display Context"
      #define E4_RESULT_DEL     "Deleting Display Context"
      #define E4_RESULT_END     "Ending Document"
      #define E4_RESULT_G4D     "G4DISPLAY buffer missing"
      #define E4_RESULT_G4E     "G4EDIT buffer missing"
      #define E4_RESULT_G4L     "G4LIST buffer missing"
      #define E4_RESULT_ON      "On New Frame"
      #define E4_RESULT_SET     "Setting Font"
      #define E4_RESULT_STA     "Starting Print Document"
      #define E4_MESSAG_EXI     "EXITING APPLICATION"
      #define E4_MESSAG_CAN     "Cancel"
      #define E4_MESSAG_OK      "OK"
      #define E4_MESSAG_PAC     "Are you sure you want to pack ?"
      #define E4_MESSAG_REC     "RECORD NUMBER"
      #define E4_MESSAG_REI     "Are you sure you want to reindex ?"
      #define E4_PARM_ADD       "Choice length must be > 0"
      #define E4_PARM_SA        "Invalid G4DISPLAY Pointer"
      #define E4_PARM_VIS       "WS_VISIBLE Style not Present"
      #define E4_PARM_WIN       "Invalid Window Dimensions"
      #define E4_RESULT_ADD     "Add String Failed"
      #define E4_RESULT_BUF     "G4LIST Buffer Not Big Enough"
      #define E4_RESULT_CDC     "CreateCompatibleDC() Failed"
      #define E4_RESULT_ENW     "EnableWindow() Failed"
      #define E4_RESULT_GDC     "GetDC() Failed"
      #define E4_RESULT_GTM     "GetTextMetrics Failed"
      #define E4_RESULT_ID      "Invalid Id Number"
      #define E4_RESULT_IDB     "Not A G4BUTTON Id Number"
      #define E4_RESULT_IDC     "Not A G4COMBO Id Number"
      #define E4_RESULT_IDD     "Not A G4DISPLAY Id Number"
      #define E4_RESULT_IDL     "Not A G4LIST Id Number"
      #define E4_RESULT_IDT     "Not A G4EDIT Id Number"
      #define E4_RESULT_PIC     "Bad Picture"
      #define E4_RESULT_POS     "Bad Caret Position"
      #define E4_RESULT_RDC     "ReleaseDC() Failed"
      #define E4_RESULT_RES     "FindResource() Failed"
      #define E4_RESULT_TA1     "No WM_TABSTOP Style Present"
      #define E4_RESULT_TA2     "Control Cannot Have The WM_TABSTOP Style"
      #define E4_RESULT_WIN     "CreateWindow() Failed"
   #endif
#endif  /* ifdef S4SWEDISH */

#ifdef S4FINNISH
   /* e4() messages */
   #define  E4_CLOSE         "Unknown Name"
   #define  E4_CREATE_FIL    "File Name:"
   #define  E4_CREATE_TEM    "Temporary Working File"
   #define  E4_CREATE_TOO    "Too Many Fields"
   #define  E4_ERROR         "Error"
   #define  E4_ERROR_NUM     "\r\n\r\nError Number"
   #define  E4_ERROR_SEV     "\r\n\r\nSevere Error Number"
   #define  E4_ERROR_BAS     "CODEBASIC ERROR"
   #define  E4_ERROR_COD     "CODEBASE ERROR"
   #define  E4_ERROR_CDS     "CODEBASE SEVERE ERROR"
   #define  E4_ERROR_ENT     "\r\nPress Enter ..."
   #define  E4_ERROR_KEY     "\r\nPress a key ..."
   #define  E4_EXPR_OM       "expression source length > max allowable chars for index"
   #define  E4_INDEX_COR     "Corrupted Free Block Chain in Index File"
   #define  E4_INDEX_EXP     "Expected '.CDX' file is of the '.IDX' type"
   #define  E4_INDEX_FIL     "Filter should be Logical"
   #define  E4_INFO_AME      "A memo block is used twice."
   #define  E4_INFO_BAC      "Backwards skip attempted without calling relate4skip_enable()"
   #define  E4_INFO_CAL      "Call to d4reccount() in r4reindex_blocks_alloc() failed"
   #define  E4_INFO_CLO      "Error closing group file in i4open()"
   #define  E4_INFO_DAT      "Database is empty."
   #define  E4_INFO_EMF      "Expected memo field non-existant in f4memo_read_low()"
   #define  E4_INFO_IFT      "f4assign_field() - invalid field type"
   #define  E4_INFO_INC      "Incorrect Record Number in tag:"
   #define  E4_INFO_KEY      "Error in Key Evaluation"
   #define  E4_INFO_LOC      "Error locking in i4open()"
   #define  E4_INFO_REA      "Error reading group file in i4open()"
   #define  E4_INFO_REC      "Record is missing."
   #define  E4_INFO_REL      "relate4top() must be called first"
   #define  E4_INFO_REP      "Repeated Record Number in tag:"
   #define  E4_INFO_SET      "Set CODE4.auto_open to 'off'"
   #define  E4_INFO_SKL      "CODE4->stored_key_len too small"
   #define  E4_INFO_SKI      "Skip must be made from valid record number"
   #define  E4_INFO_TAG      "Tag is corrupted"
   #define  E4_LOCK_LOC      "Attempt to lock file that is read-only"
   #define  E4_LOCK_UNL      "Attempt to unlock file that is read-only"
   #define  E4_MEMORY_B      "Block Size too Large for Indexing"
   #define  E4_MEMORY_S      "Sorting"
   #define  E4_NUM_PARMS     "Function"
   #define  E4_OPT_INV       "Invalid mode"
   #define  E4_RESULT_CII    "Clipper incompatible Index File detected."
   #define  E4_RESULT_D4F    "d4flush_record() did not work."
   #define  E4_RELATE_EAI    "RELATE4->error_action invalid"
   #define  E4_RELATE_MEN    "RELATE4->master expected is null"
   #define  E4_RELATE_RCS    "relate4create_slave() - master expression type does not match tag type"
   #define  E4_RELATE_REL    "relate4match_len() can only be called with a character expression"
   #define  E4_REPORT_FILE   "This file is not a CodeReporter report file."
   #define  E4_REPORT_ONE    "Only one report can be loaded at a time"
   #define  E4_REPORT_DFILE  "Unable to Open Data File"
   #define  E4_REPORT_GRO    "Group Not Found while Retrieving Report"
   #define  E4_REPORT_IFILE  "Index File Not Found"
   #define  E4_REPORT_ASTYLE "Unable to Allocate Memory For Style"
   #define  E4_REPORT_AGROUP "Unable to Allocate Memory For Group"
   #define  E4_REPORT_RGROUP "Unable to Retrieve Group Information"
   #define  E4_REPORT_ATOTAL "Unable to Allocate Memory for Total"
   #define  E4_REPORT_ATEXT  "Unable to Allocate Memory for Text Object"
   #define  E4_REPORT_HINST  "'hInst' member of CODE4 is not Initialized"
   #define  E4_REPORT_HWND   "'hWndParent' member of REPORT4 is not Initialized"
   #define  E4_REPORT_RTEXT  "Unable to Retrieve Text Object Information"
   #define  E4_REPORT_RCALC  "Unable to Retrieve Calculation Information"
   #define  E4_REPORT_RTOTAL "Unable to Retrieve Total Information"
   #define  E4_REPORT_TRG    "Unable to Locate Reset Group"
   #define  E4_REPORT_ROBJS  "Unable to Retrieve Objects Information"
   #define  E4_RESULT_DAT    "Database not found"
   #define  E4_RESULT_EXP    "Expecting logical result."
   #define  E4_RESULT_IDE    "Identical keys in unique key tag:"
   #define  E4_RESULT_INT    "Internal buffer too small while retrieving report" 
   #define  E4_RESULT_LCF    "Could not locate File:"
   #define  E4_RESULT_LOS    "Lost Block in Index File"
   #define  E4_RESULT_REC    "Record numbers out of order for equal keys:"
   #define  E4_RESULT_TAG    "Tag is out of date:"
   #define  E4_RESULT_THE    "The tag is out of order:"
   #define  E4_RESULT_UNE    "Unexpected key evaluation length:"
   #define  E4_RESULT_WAS    "Wasted Space in Memo File"
   #define  E4_PARM_MUS      "relate4top() must be called first"
   #define  E4_PARM_OPT      "CODE4 optimization initialization values too small" 
   #define  E4_PARM_REL      "Called relate4create_slave() with a database that already exists in another part of the relation" 
   #define  E4_PARM_REP      "Attempt to replace to a read-only file" 
   #define  E4_PARM_SIZ      "Attempt to change file length of a read-only file"
   #define  E4_TYPE_SUB      "Expression:"
   #define  E4_PARM_TAA      "i4add_tag() - an input tag already exists in the index file"
   #define  E4_TYPE_UFT      "Unknown field type"
   #define  E4_PARM_WRT      "Attempt to write to a read-only file" 
   #define  E4_UNIQUE        "Creating Tag"

   /* e4severe() messages */
   #define  E4_BM4_IEL       "bitmap4seek() - invalid expression length"
   #define  E4_BM4_IM        "bitmap4combine_leafs() - incompatible maps"
   #define  E4_CONST_EIN     "const4get() - expression incongruity"
   #define  E4_DATA_ILL      "Illegal field data."
   #define  E4_DATA_COR      "Corrupt Database File."
   #define  E4_DATA_DEL      "d4delete() - invalid deletion flag detected"
   #define  E4_DATA_DELD     "d4deleted() - invalid deletion flag detected"
   #define  E4_DATA_MEM      "Memo File not Open for Append"
   #define  E4_DATA_UPDATE   "Number records unknown at update time"
   #define  E4_DATA_RECALL   "d4recall() - invalid deletion flag"
   #define  E4_EXPR_DELETED  "e4deleted() - invalid value detected in record buffer's deleted flag"
   #define  E4_INFO_ACC      "Accessing NULL Block"
   #define  E4_INFO_ALR      "Already initialized"
   #define  E4_INFO_BDC      "Corrupt NTX Clipper index file"
   #define  E4_INFO_BDI      "Corrupt IDX FoxPro index file"
   #define  E4_INFO_BLO      "Block size is less than 1024 bytes"
   #define  E4_INFO_BLS      "This is unsupported since dBASE IV compatibility requires minimum blocks sizes of 1024 bytes"
   #define  E4_INFO_BMC      "Block/memory corruption"
   #define  E4_INFO_C4C      "'CODE4.mem_size_block' is an illegal value."
   #define  E4_INFO_COD      "CodeBase internal error"
   #define  E4_INFO_CIF      "Corrupt index file"
   #define  E4_INFO_CIB      "Corrupt index block"
   #define  E4_INFO_CIS      "Corrupt index structure"
   #define  E4_INFO_CMF      "Corrupt memo file"
   #define  E4_INFO_CRL      "Corrupted Linked List"
   #define  E4_INFO_DAO      "Database file already open"
   #define  E4_INFO_DEL      "Deleted block not empty"
   #define  E4_INFO_DUP      "Duplicate Alias"
   #define  E4_INFO_EPV      "Expected value not built"
   #define  E4_INFO_EVA      "Evaluating Tag Expression"
   #define  E4_INFO_EXK      "Expression key is larger than maximum 102 characters"
   #define  E4_INFO_EXP      "Expression too large"
   #define  E4_INFO_FAI      "Failure in logical field index"
   #define  E4_INFO_FOR      "For tag"
   #define  E4_INFO_ILF      "Illegal file block reference"
   #define  E4_INFO_ILL      "Illegal locking configuration"
   #define  E4_INFO_ILP      "Illegal position"
   #define  E4_INFO_IMS      "Invalid memo size in memo4file_write()"
   #define  E4_INFO_INT      "Internal t4remove_current() failure--index file corrupt"
   #define  E4_INFO_INV      "Invalid key type in index file"
   #define  E4_INFO_IAO      "Index file already open"
   #define  E4_INFO_IVE      "Invalid error action"
   #define  E4_INFO_IVT      "Invalid type"
   #define  E4_INFO_LIN      "Link not on list"
   #define  E4_INFO_NKE      "n_keys invalid"
   #define  E4_INFO_STR      "Function was Passed an Invalid Structure Pointer"
   #define  E4_INFO_TAO      "Tag file already open"
   #define  E4_INFO_UNE      "Unexpected empty block"
   #define  E4_INFO_UNI      "Un-initialized values in opt4add_block"
   #define  E4_INFO_UNT      "Unexpected tag locks in index file"
   #define  E4_INFO_WRO      "Wrong number of links in linked list"
   #define  E4_INFO_WT4      "Wrong TAG4 Type"
   #define  E4_MEMORY_ERR    "Memory Error"
   #define  E4_MEMORY_OOR    "Memory out of range in const4mem_alloc()"
   #define  E4_MEMORY_SAV    "l4lock_save(), Saving Lock Information"
   #define  E4_MEMORY_YPO    "mem4pop_pointer() with S4DEBUG switch."
   #define  E4_MEMORY_YPU    "mem4push_pointer() with S4DEBUG switch."
   #define  E4_OPEN_NOD      "i4open(): No database specified"
   #define  E4_PARM_FLA      "Flag should be 'r4descending'."
   #define  E4_PARM_FOR      "For S4NDX version, filter and descending should be 0"
   #define  E4_PARM_ILP      "Illegal position"
   #define  E4_PARM_IND      "Index file already open"
   #define  E4_PARM_INV      "Invalid lock position"
   #define  E4_PARM_NFD      "relate4sort(): Non-freed data list"
   #define  E4_PARM_NSD      "Null String Detected"
   #define  E4_PARM_OSD      "Overlapping Strings Detected"
   #define  E4_PARM_NUL      "Null Data File Pointer"
   #define  E4_PARM_REC      "Record Numbers are not Consecutive"
   #define  E4_PARM_TAG      "Tag expression is missing."
   #define  E4_PARM_TOO      "Too many tags"
   #define  E4_PARM_UNI      "Unique flag data is an unrecognized value."
   #define  E4_PARM_ZER      "Zero parameter"
   #define  E4_RESULT_CLI    "Clipper Key Conversion failed"
   #define  E4_RESULT_CMP    "mem4check_pointer() - corrupt memory pointer encountered"
   #define  E4_RESULT_COM    "Corrupted Memory Detected"
   #define  E4_RESULT_COR    "Corrupted Index File in Check"
   #define  E4_RESULT_D4A    "d4append_start() was not completed."
   #define  E4_RESULT_D4I    "d4init() has not been called."
   #define  E4_RESULT_EXI    "Exiting from within lock wait"
   #define  E4_RESULT_FRE    "Free value not found"
   #define  E4_RESULT_INC    "Incorrect index file locking order."
   #define  E4_RESULT_INQ    "In quick sort."
   #define  E4_RESULT_LOC    "Locking index could create deadlock."
   #define  E4_RESULT_LCO    "Locking Overlap Detected"
   #define  E4_RESULT_MEM    "Memory items not freed"
   #define  E4_RESULT_REM    "Removing Lock which was never placed."
   #define  E4_RESULT_S4L    "S4LOCK_CHECK cannot be used with 'mem4reset'"
   #define  E4_RESULT_STC    "Stack length variable _stklen below minimum."
   #define  E4_RESULT_TOO    "Too many pointers"

   #ifdef S4WINDOWS
      #define E4_REPORT_BIT     "Could not create a bitmap"
      #define E4_REPORT_COD     "CodeReporter"
      #define E4_REPORT_CRE     "Could not create output window"
      #define E4_REPORT_DIS     "Could not get a display context"
      #define E4_REPORT_FON     "Error Creating Font"
      #define E4_REPORT_PRI     "Printing"
      #define E4_RESULT_CRE     "Creating Printer Display Context"
      #define E4_RESULT_DEL     "Deleting Display Context"
      #define E4_RESULT_END     "Ending Document"
      #define E4_RESULT_G4D     "G4DISPLAY buffer missing"
      #define E4_RESULT_G4E     "G4EDIT buffer missing"
      #define E4_RESULT_G4L     "G4LIST buffer missing"
      #define E4_RESULT_ON      "On New Frame"
      #define E4_RESULT_SET     "Setting Font"
      #define E4_RESULT_STA     "Starting Print Document"
      #define E4_MESSAG_EXI     "EXITING APPLICATION"
      #define E4_MESSAG_CAN     "Cancel"
      #define E4_MESSAG_OK      "OK"
      #define E4_MESSAG_PAC     "Are you sure you want to pack ?"
      #define E4_MESSAG_REC     "RECORD NUMBER"
      #define E4_MESSAG_REI     "Are you sure you want to reindex ?"
      #define E4_PARM_ADD       "Choice length must be > 0"
      #define E4_PARM_SA        "Invalid G4DISPLAY Pointer"
      #define E4_PARM_VIS       "WS_VISIBLE Style not Present"
      #define E4_PARM_WIN       "Invalid Window Dimensions"
      #define E4_RESULT_ADD     "Add String Failed"
      #define E4_RESULT_BUF     "G4LIST Buffer Not Big Enough"
      #define E4_RESULT_CDC     "CreateCompatibleDC() Failed"
      #define E4_RESULT_ENW     "EnableWindow() Failed"
      #define E4_RESULT_GDC     "GetDC() Failed"
      #define E4_RESULT_GTM     "GetTextMetrics Failed"
      #define E4_RESULT_ID      "Invalid Id Number"
      #define E4_RESULT_IDB     "Not A G4BUTTON Id Number"
      #define E4_RESULT_IDC     "Not A G4COMBO Id Number"
      #define E4_RESULT_IDD     "Not A G4DISPLAY Id Number"
      #define E4_RESULT_IDL     "Not A G4LIST Id Number"
      #define E4_RESULT_IDT     "Not A G4EDIT Id Number"
      #define E4_RESULT_PIC     "Bad Picture"
      #define E4_RESULT_POS     "Bad Caret Position"
      #define E4_RESULT_RDC     "ReleaseDC() Failed"
      #define E4_RESULT_RES     "FindResource() Failed"
      #define E4_RESULT_TA1     "No WM_TABSTOP Style Present"
      #define E4_RESULT_TA2     "Control Cannot Have The WM_TABSTOP Style"
      #define E4_RESULT_WIN     "CreateWindow() Failed"
   #endif
#endif  /* S4FINNISH */
#endif  /* S4LANGUAGE  */
#endif  /* S4OFF_ERROR */

typedef struct error_data_st
{
   int   error_num ;
   char S4PTR *error_data ;
}  ERROR_DATA ;
