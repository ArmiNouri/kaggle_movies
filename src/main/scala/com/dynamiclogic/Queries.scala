package com.dynamiclogic

/**
 * Created by armineh on 7/15/14.
 */

import scala.slick.lifted.Query
import scala.slick.jdbc.{GetResult, StaticQuery => Q}
import Q.interpolation
import scala.slick.jdbc.StaticQuery
import scala.slick.jdbc.JdbcBackend.{Session => PSession}
import scala.slick.driver.MySQLDriver.simple._

case class TrainToken(phrase_id: Long, sentence_id: Long, tokens: String, sentiment: Int)

object Queries {
  import Kaggle._
   def get_train_tokens(tb: String)(implicit s:PSession): Seq[TrainToken] = {
     (for{
       dt <- Train(tb)
       t <- Tokens(tb)
       if dt.phrase_id === t.tweet_id
     } yield {
       (dt.phrase_id, dt.sentence_id, t.tokens, dt.sentiment)
     }).list.map(t => TrainToken(t._1, t._2, t._3, t._4))
   }

  def get_training(tb: String)(implicit s:PSession): Seq[Example] = {
    Kaggle.Train(tb).list
  }

  def store_raw(table: String, f: java.io.File)(implicit s:PSession) = {
    val query: String  = "LOAD DATA LOCAL INFILE '" + f.getPath() + "' into table " + Kaggle.get_trainingraw_name(table) + " fields terminated by ',' enclosed by '''' lines terminated by '\n' (dataset_id,token,bucket,number,count)"
    println(query)
    (Q.u + query).execute
  }

  def store_counts(table: String)(implicit s:PSession) = {
    val query: String = "insert into " + Kaggle.get_trainingcounts_name(table) + " select dataset_id, token, bucket, number, sum(count), NULL tifdf from " + Kaggle.get_trainingraw_name(table) + " group by dataset_id,token,number"
    (Q.u + query).execute
  }

  def get_train_counts(tb: String)(implicit s:PSession) = {
    Kaggle.TrainingTfidfCOUNTS(tb).list
  }

  def update_tfidf(table: String, f: java.io.File)(implicit s:PSession) = {
    val query: String  = "LOAD DATA LOCAL INFILE '" + f.getPath() + "' REPLACE into table " + Kaggle.get_trainingcounts_name(table) + " fields terminated by ',' enclosed by '''' lines terminated by '\n' (dataset_id,token,bucket,number,count,tfidf)"
    println(query)
    (Q.u + query).execute
  }

  def get_training_examples(tb: String, limit: Int)(implicit s:PSession): Seq[Example] = {
    //val query: String = "SELECT tr.phrase_id, tr.sentence_id, tk.tokens, tr.sentiment FROM " + Kaggle.get_train_name(tb) + " tr INNER JOIN " + Kaggle.get_tokens_name(tb) + " tk ON (tr.phrase_id = tk.tweet_id) ORDER BY RAND() LIMIT " + limit
    val query: String = "SELECT * FROM " + Kaggle.get_train_name(tb) + " ORDER BY RAND() LIMIT " + limit
    val query_rs: Seq[(Long, Long, String, Int)] = Q.queryNA[(Long, Long, String, Int)](query).list

    query_rs.map(r => Example(r._1, r._2, r._3, r._4))
  }

  def get_tokens(tb: String, phrase_id: Long)(implicit s:PSession): String = {
    (for { t <- Tokens(tb) if t.tweet_id === phrase_id } yield t.tokens).list.head
  }

  def get_tfidf(tb: String, tokens: List[String])(implicit s:PSession): Seq[TrainingTfidfRow] = {
    //(for {t <- TrainingTfidfCOUNTS(tb)  if t.token inSet tokens } yield t).list
    val query: String = "SELECT * FROM " + Kaggle.get_trainingcounts_name(tb) + " WHERE token in ( " + tokens.map(x => s"'$x'").mkString(", ") + " )"
    if(tokens.size > 0 ) {
      val query_rs: Seq[(String, Option[String], String, Int, Int, Option[Double])] = Q.queryNA[(String, Option[String], String, Int, Int, Option[Double])](query).list
      query_rs.map(x => TrainingTfidfRow(x._1, x._2, x._3, x._4, x._5, x._6))
    }
    else Seq.empty
  }

  def get_prob_sent(tb: String, sentiment: Int)(implicit s:PSession): Int  = {
    Kaggle.Train(tb).filter(_.sentiment === sentiment).list.size
  }

  def get_train_size(tb: String)(implicit s:PSession): Int = {
    Kaggle.Train(tb).list.size
  }
}
