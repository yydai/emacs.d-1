;;A simple Hive mode for emacs derived from SQL mode.

;;Define regex for various classes of hive keywords
(setq hive-constants '("null"))
(setq hive-constants-regex (regexp-opt hive-constants 'words))
(setq hive-constants nil)

(setq hive-datatypes '("tinyint" "smallint" "int" "bigint" "boolean" "float" "double" "string" "binary" "timestamp" "decimal" "varchar" "char" "array" "struct" "uniontype" "textfile" "sequencefile" "rcfile" "inputformat"))
(setq hive-datatypes-regex (regexp-opt hive-datatypes 'words))
(setq hive-datatypes nil)

(setq hive-keywords-ddl '("create" "database" "schema" "comment" "if" "not" "exists" "location" "drop" "restrict" "cascade" "alter" "set" "table" "external" "partitioned" "by" "clustered" "skewed" "row" "format" "delimited" "stored" "as" "like" "partition" "to" "replace" "view" "index" "on" "temporary" "function" "show" "tables" "describe" "partitions"))
(setq hive-keywords-dml '("add" "touch" "load" "data" "overwrite" "into" "insert" "select" "from" "where" "join" "semi" "left" "right" "outer" "union" "all" "group" "order" "is" "and" "or"))
(setq hive-allkeywords-regex (regexp-opt (append hive-keywords-ddl hive-keywords-dml) 'words ))
(setq hive-keywords-ddl nil)
(setq hive-keywords-dml nil)


;;Define font face for keywords
(setq hive-font-lock-keywords
      `(
	(,hive-constants-regex . font-lock-constant-face)
	(,hive-datatypes-regex . font-lock-type-face)
	(,hive-allkeywords-regex . font-lock-keyword-face)
))

;;Define Hive mode
(define-derived-mode hive-mode sql-mode
  "Hive mode"
  "Basic hive mode derived from SQL mode"

  (setq font-lock-defaults '((hive-font-lock-keywords)))
  
  (setq hive-constants-regex nil)
  (setq hive-datatypes-regex nil)
  (setq hive-allkeywords-regex nil)
)

(provide 'init-hive)

