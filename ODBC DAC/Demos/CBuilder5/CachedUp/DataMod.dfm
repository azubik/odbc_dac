�
 TCACHEDATA 0�  TPF0
TCacheData	CacheDataOldCreateOrder	LeftTop� Height�Width� TDataSource
DataSourceDataSet	ODBCQueryLeft&TopF  TODBCConnectionODBCConnectionEnvironmentDefault.OwnerLoginPromptLeft(Top  
TODBCQuery	ODBCQuery
ConnectionODBCConnectionOnCalcFieldsODBCQueryCalcFieldsBufferChunks CachedUpdates
ParamCheck	SQL.StringsSELECT  customer_num,  fname,  lnameFROM
  customer Left� Top TAutoIncFieldODBCQuerycustomer_num	FieldNamecustomer_numRequired	  TStringFieldODBCQueryfname	FieldNamefname	FixedChar	Size  TStringFieldODBCQuerylname	FieldNamelname	FixedChar	Size  TStringFieldODBCQueryUpdateStatus	FieldKindfkCalculated	FieldNameUpdateStatus
Calculated	   TODBCUpdateSQLODBCUpdateSQLModifySQL.StringsUPDATE customerSET -- customer_num = :customer_num,  fname = :fname,  lname = :lnameWHERE"  customer_num = :old_customer_num InsertSQL.StringsINSERT INTO customer  (customer_num, fname, lname)VALUES!  (:customer_num, :fname, :lname) DeleteSQL.StringsDELETE FROM
  customerWHERE"  customer_num = :old_customer_num CheckRowsAffected	Left� Top   