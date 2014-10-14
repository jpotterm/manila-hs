{-# LANGUAGE OverloadedStrings #-}

module Mint.Category (mintCategories) where

import Control.Lens
import Data.List
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import Database.HDBC
import Database.HDBC.Sqlite3
import qualified Network.Wreq as Wreq
import Text.XML.HXT.Core

import Settings
import Util


mintCategories :: Wreq.Options -> IO ()
mintCategories session = do
    let categoryUrl = Settings.mintHostname ++ "/popup.xevent?task=categories"
    categories <- Wreq.getWith session categoryUrl

    getCategories $ LT.unpack $ LTE.decodeUtf8 $ categories ^. Wreq.responseBody


getCategories :: String -> IO ()
getCategories categoriesString = do
    conn <- getDbConnection

    categories <- extractCategories categoriesString
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
