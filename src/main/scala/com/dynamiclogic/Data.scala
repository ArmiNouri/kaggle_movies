package com.dynamiclogic

import org.joda.time._
import scala.slick.jdbc.JdbcBackend.{Session => PSession}
import scala.slick.driver.MySQLDriver.simple._

/**
 * Created by armineh on 7/22/14.
 */


object Kaggle {
  def TrainingTfidfRAW(tb: String) = TableQuery[TrainingTfidf]((tag:Tag) => new TrainingTfidf(tag, get_trainingraw_name(tb)))
  def TrainingTfidfCOUNTS(tb: String) = TableQuery[TrainingTfidf]((tag:Tag) => new TrainingTfidf(tag, get_trainingcounts_name(tb)))
  def Train(tb: String) = TableQuery[Train]((tag:Tag) => new Train(tag, get_train_name(tb)))
  def Test(tb: String) = TableQuery[Test]((tag:Tag) => new Test(tag, get_test_name(tb)))
  def Tokens(tb: String) = TableQuery[Tokens]((tag: Tag) => new Tokens(tag, get_tokens_name(tb)))
  def DatasetTweets(tb: String) = TableQuery[DatasetTweets]((tag: Tag) => new DatasetTweets(tag, get_dataset_tweets_name(tb)))

  def get_trainingraw_name(tb: String): String = tb + "_training_tfidf_1"
  def get_trainingcounts_name(tb: String): String = tb + "_training_tfidf_2"
  def get_train_name(tb: String): String = tb + "_train"
  def get_test_name(tb: String): String = tb + "_test"
  def get_tokens_name(tb: String): String = tb + "_tokens"
  def get_dataset_tweets_name(tb: String): String = tb + "_dataset_tweets"
  def get_results_name(tb: String): String = tb + "_results"
}

case class TrainingTfidfRow(dataset_id: String, token: Option[String] = None, bucket: String, number: Int, count: Int, tfidf: Option[Double] = None)

class TrainingTfidf (tag: Tag, tableName: String) extends Table[TrainingTfidfRow](tag, tableName) {
  //import BigIntMapper.BigIntTypeMapper

  def dataset_id = column[String]("dataset_id")
  def token = column[String]("token")
  def bucket = column[String]("bucket")
  def number = column[Int]("number")
  def count = column[Int]("count")
  def tfidf = column[Double]("tfidf")

  def * = (dataset_id , token.? , bucket , number, count, tfidf.?) <> (TrainingTfidfRow.tupled, TrainingTfidfRow.unapply)
}

case class Example(phrase_id: Long, sentence_id: Long, phrase: String, sentiment: Int)

class Train (tag: Tag, tableName: String) extends Table[Example](tag, tableName) {
  //import BigIntMapper.BigIntTypeMapper

  def phrase_id = column[Long]("phrase_id")
  def sentence_id = column[Long]("sentence_id")
  def phrase = column[String]("phrase")
  def sentiment = column[Int]("sentiment")

  def * = (phrase_id , sentence_id , phrase , sentiment) <> (Example.tupled, Example.unapply)
}

class Test (tag: Tag, tableName: String) extends Table[Example](tag, tableName) {
  //import BigIntMapper.BigIntTypeMapper

  def phrase_id = column[Long]("phrase_id")
  def sentence_id = column[Long]("sentence_id")
  def phrase = column[String]("phrase")
  def sentiment = column[Int]("sentiment")

  def * = (phrase_id , sentence_id , phrase , sentiment) <> (Example.tupled, Example.unapply)
}

case class Token(tweet_id: Long, tokens: Option[String])

class Tokens (tag: Tag, tableName: String) extends Table[Token](tag, tableName) {
  //import BigIntMapper.BigIntTypeMapper

  def tweet_id = column[Long]("tweet_id", O.PrimaryKey)
  def tokens = column[String]("tokens")

  def * = (tweet_id , tokens.?) <> (Token.tupled, Token.unapply)
}

case class DatasetTweet(dataset_id: String, tweet_id: Long, cleaned: Boolean, keep: Boolean, whitelisted: Boolean, blacklisted: Boolean, tweet_type: Int)

class DatasetTweets (tag: Tag, tableName: String) extends Table[DatasetTweet](tag, tableName) {
  def dataset_id = column[String]("dataset_id", O.PrimaryKey)
  def tweet_id = column[Long]("tweet_id", O.PrimaryKey)
  def cleaned = column[Boolean]("cleaned")
  def keep = column[Boolean]("keep")
  def whitelisted = column[Boolean]("whitelisted")
  def blacklisted = column[Boolean]("blacklisted")
  def tweet_type = column[Int]("tweet_type")

  def *  = (dataset_id, tweet_id, cleaned, keep, whitelisted, blacklisted, tweet_type) <> (DatasetTweet.tupled, DatasetTweet.unapply)
}