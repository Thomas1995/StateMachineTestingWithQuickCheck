{-# LANGUAGE OverloadedStrings #-}

import Database.MongoDB ( Field
                        , access
                        , connect
                        , host
                        , insert
                        , master
                        , (=:)
                        )

import Test.QuickCheck ( Arbitrary
                       , Args
                       , Gen
                       , Property
                       , arbitrary
                       , maxSuccess
                       , quickCheckWith
                       , stdArgs
                       , elements
                       , listOf1
                       , vectorOf
                       )

import Test.QuickCheck.Monadic ( assert
                               , monadicIO
                               , run
                               )

import qualified System.IO.Streams as Streams ( fromList )

maxCustomListSize :: Int
maxCustomListSize = 10000000

minCustomListSize :: Int
minCustomListSize = 1000

data CustomPost = MkCustomPost { post :: [Field] }
  deriving (Show)

data CustomPostList = MkCustomPostList [CustomPost]
  deriving (Show)

instance Arbitrary CustomPost where
 arbitrary = do
   value <- listOf1 genSafeChar
   return $ MkCustomPost $ ["value" =: value]
  where
   genSafeChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

instance Arbitrary CustomPostList where
 arbitrary = do
   size <- arbitrary
   list <- vectorOf (size `mod` maxCustomListSize + minCustomListSize) (arbitrary :: Gen CustomPost)
   return $ MkCustomPostList list

customArgs :: Args
customArgs = ( stdArgs { maxSuccess = 1000000000 } )

mongoDBHasExpectedBehavior pipe instanceOfCustomPostList = monadicIO $ do
  realityMatchesModel <- run $ do
    let MkCustomPostList listOfCustomPosts = instanceOfCustomPostList

    listOfActions <- mapM mapAction listOfCustomPosts
    myStream <- Streams.fromList listOfActions

    writeFile "history.log" $ show (length $ listOfCustomPosts) ++ " insertions\n"

    return $ (1==0)

  assert $ realityMatchesModel

  where
    mapAction customPost = do
      access pipe master dataBaseName $ insert dataBaseName $ post customPost


dataBaseName = "test"
serverAddress = "127.0.0.1"

main :: IO ()
main = do
  pipe <- connect $ host serverAddress
  quickCheckWith customArgs (mongoDBHasExpectedBehavior pipe)
