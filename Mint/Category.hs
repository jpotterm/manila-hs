{-# LANGUAGE OverloadedStrings #-}

module Mint.Category (getCategoriesRequest) where

import Control.Monad.Trans
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy.Char8
import qualified Data.ByteString.Lazy as L
import Data.List
import Database.HDBC
import Database.HDBC.Sqlite3
import Network.HTTP.Conduit
import Network.HTTP.Conduit.Browser
import Text.XML.HXT.Core

import Utils


getCategoriesRequest :: BrowserAction ()
getCategoriesRequest = do
    request <- parseUrl "https://wwws.mint.com/popup.xevent?task=categories"
    response <- makeRequestLbs request

    liftIO $ getCategories response


getCategories :: Response L.ByteString -> IO ()
getCategories response = do
    conn <- getDbConnection

    categories <- extractCategories $ Data.ByteString.Lazy.Char8.unpack (responseBody response)
    commit conn

    categoryIds <- mapM (saveCategory conn) categories
    let flatCategoryIds = concat categoryIds
    let questionMarks = intersperse ',' $ replicate (length flatCategoryIds) '?'

    run conn ("DELETE FROM category WHERE id NOT IN (" ++ questionMarks ++ ")") (map toSql flatCategoryIds)

    commit conn
    disconnect conn


saveCategory :: Connection -> (String, [String]) -> IO [Integer]
saveCategory conn (category, subcategories) = do
    run conn "INSERT INTO category (name) VALUES (?)" [toSql category]
    commit conn

    categoryId <- getCategoryId conn category
    subcategoryIds <- mapM (saveSubcategory conn categoryId) subcategories

    return (categoryId:subcategoryIds)


saveSubcategory :: Connection -> Integer -> String -> IO Integer
saveSubcategory conn categoryId subcategory = do
    result <- getOrCreate conn
        ("SELECT id FROM category WHERE name = ? AND parent_id = ?", [toSql subcategory, toSql categoryId])
        ("INSERT INTO category (name, parent_id) VALUES (?, ?)", [toSql subcategory, toSql categoryId])

    return . fromSql . head . head $ result


extractCategories :: String -> IO [(String, [String])]
extractCategories contents = do
    let doc = readString [withParseHTML yes, withWarnings no] contents

    let firstLis = doc
            //> hasName "ul" >>> hasAttrValue "id" (== "popup-cc-L1")
            /> hasName "li" >>> hasAttrValue "class" (== "isL1")

    runX $ firstLis >>> (getCategoryName &&& getSubcategories)


getSubcategories :: IOSArrow XmlTree [String]
getSubcategories = listA (getNormalSubcategories <+> getCustomSubcategories)


getNormalSubcategories :: IOSArrow XmlTree String
getNormalSubcategories =
    getChildren >>> hasAttrValue "class" (== "popup-cc-L2")
    /> hasName "ul"
    /> hasName "li" >>> removeAllWhiteSpace
    /> getText


getCustomSubcategories :: IOSArrow XmlTree String
getCustomSubcategories =
    getChildren >>> hasAttrValue "class" (== "popup-cc-L2")
    /> hasName "ul" >>> hasAttrValue "class" (== "popup-cc-L2-custom")
    /> hasName "li"
    /> hasName "input" >>> hasAttrValue "name" (== "nameOfTag") >>> hasAttrValue "value" (/= "")
    >>> getAttrValue "value"


getCategoryName :: IOSArrow XmlTree String
getCategoryName = getChildren >>> hasName "a" >>> removeAllWhiteSpace /> getText
