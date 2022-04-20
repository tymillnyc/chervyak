{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib where
    	
import System.IO
import Text.Read
import Database.HDBC.Sqlite3 (connectSqlite3, Connection())
import Database.HDBC

type Name = String
type Result = Int
type ID = Int

--create user, if it doesn't exist, return id
createAUser:: Name -> Connection -> IO (Int)
createAUser name conn = do
	selectPreparetion <- prepare conn "SELECT id from users where name = ?"
	res <- execute selectPreparetion [toSql name] 
	results <- fetchAllRowsAL selectPreparetion
	if null results then do
		run conn "INSERT INTO users (name) VALUES (?)" [toSql name]
		commit conn
		newUserId <- (createAUser name conn)
		return newUserId
	else do
		return (getIDFromResult results)
		
--type conversion id to int
getIDFromResult:: [[(String, SqlValue)]] -> Int
getIDFromResult mas = fromSql (snd(head(head mas)))
getIDFromResult  _ = -1

getConnection:: IO Connection
getConnection = do
	conn <- connectSqlite3 "dbChervyak"
	run conn "CREATE TABLE if not exists users (id integer PRIMARY KEY AUTOINCREMENT NOT NULL, name varchar(27) NOT NULL)" []
	run conn "CREATE TABLE if not exists records (id integer NOT NULL, result int NOT NULL)" []
	commit conn
	return conn

someFunc:: IO()
someFunc = do
	conn <- getConnection
	s <- createAUser "alexs" conn 
	putStrLn (show s)
	--writeResultToTable 440 s conn
	d <- getTop10 conn
	disconnect conn
	putStrLn (show d)




--write the result to the table of records
writeResultToTable:: Result -> ID -> Connection -> IO ()
writeResultToTable result id conn = do
	run conn "INSERT INTO records (id, result) VALUES (?, ?)" [toSql id, toSql result]
	commit conn

--parsing the array returned by the table
getListFromQuery::[[(String, SqlValue)]]->[(String, Int)]
getListFromQuery mas = map (\elem -> (fromSql( snd (head elem) ), fromSql( snd ( head ( tail elem ) ) ) ) ) mas


--get top 10 results from records table
getTop10:: Connection -> IO [(String, Int)]
getTop10 conn = do 
	selectPreparetion <- prepare conn "SELECT name, MAX(result) from records left join users on users.id = records.id group by name ORDER BY result DESC limit 10"
	res <- execute selectPreparetion []
	results <- fetchAllRowsAL' selectPreparetion
	return (getListFromQuery results)
