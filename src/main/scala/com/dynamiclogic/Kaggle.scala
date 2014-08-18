/**
 * Created by armineh on 7/14/14.
 */
package com.dynamiclogic
import chalk.topics._
import breeze.linalg._
import scala.slick.jdbc.JdbcBackend.{Session => PSession}
import scala.slick.jdbc.{GetResult, StaticQuery => Q}
import scala.slick.driver.MySQLDriver.simple._

import breeze.numerics._



import breeze.config.CommandLineParser
import breeze.util.Index
import java.io.File
import chalk.text.tokenize.JavaWordTokenizer
import chalk.text.transform.StopWordFilter
import scala.io._
import scala.collection.immutable.HashMap
import scala.collection.parallel.ParSeq
import breeze.util.Implicits._
import org.joda.time._
import java.io.FileWriter
import java.io.PrintWriter


import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import edu.stanford.nlp.util.CoreMap
import edu.stanford.nlp.ling.CoreAnnotations._
import scala.collection.JavaConversions._
import edu.stanford.nlp.classify._
import edu.stanford.nlp.stats.ClassicCounter
import edu.stanford.nlp.util.Pair


object Globals {

  val DB = scala.slick.jdbc.JdbcBackend.Database.forURL("jdbc:mysql://verveindex.cdd1o6al2jdl.us-east-1.rds.amazonaws.com/verveindex?characterEncoding=UTF-8", driver = "com.mysql.jdbc.Driver", user="maartenp", password="maartenp123")

//  def printToFile(f: java.io.File, b: Boolean)(op: java.io.PrintWriter => Unit) {
//    val p = new java.io.PrintWriter(new java.io.FileOutputStream(f, b))
//    try { op(p) } finally { p.close() }
//  }
  def using[A <: {def close(): Unit}, B](param: A)(f: A => B): B =
    try { f(param) } finally { param.close() }
  def writeToFile(fileName:String, data:String) =
    using (new FileWriter(fileName)) {
      fileWriter => fileWriter.write(data)
    }
  def appendToFile(fileName:String, textData:String) =
    using (new FileWriter(fileName, true)) {
      fileWriter => using (new PrintWriter(fileWriter)) {
        printWriter => printWriter.println(textData)
      }
    }
//
//  def sift(Tweet: List[String], foreign: Boolean = false): List[String] = {
//    def untag(str: String): String = if(str.size >0 && (str.head == '#' || str.head == '@')) str.tail else str
//    val unigrams: List[String] = Tweet.map( x => x.split("<:>") ).filter( x => (x.size > 1 && resolve(x(1), foreign) != "other") ).map ( x => untag(x(0).toLowerCase) )
//
//    unigrams :::
//      (for { i <- 0 until unigrams.size
//             head = unigrams(i)
//             tail = unigrams.drop(i+1)
//             g <- tail.map( t => head + "<!!>" + t)
//      }
//      yield {g}).toList
//  }
//
//  def resolve(tag: String, foreign: Boolean): String = {
//    if(List("VB", "VBD", "VBG", "VBN", "VBP", "VBZ") contains tag) "verb"
//    else if(List("NN", "NNP", "NNS", "NNPS") contains tag) "noun"
//    else if(List("JJ", "JJR", "JJS") contains tag) "adje"
//    else if(List("RB", "RBR", "RBS", "WRB") contains tag) "adve"
//    //TODO: expand your allowable POS tags
//    else if(List("DT", "IN", "TO", "PRP","PRP$","WP","WP$", "$") contains tag) "allowable extras"
//    //adding foreign language support
//    else if(foreign && tag == "FW") "allowable foreign language content"
//    else "other"
//  }

  def sift(tokens: List[String]): List[String] = {
    val unigrams: List[String] = tokens
    unigrams :::
      (for {
        i <- 0 until unigrams.size
        head = unigrams(i)
        tail = unigrams.drop(i+1)
        g <- tail.map(t => head + "<!!>" + t)
      } yield {
        {g}
      }).toList
  }

  lazy val table: String = "kaggle"
  lazy val dataset_id: String = "rt"
  lazy val group_size: Int = 50000
  lazy val directory: String = "data/"
  lazy val tokens_directory: String = "data/tokens/"
  lazy val tfidfs_directory: String = "data/tfidfs/"
  lazy val errors_directory: String = "data/errors/"
  lazy val tokens_path: String = s"${tokens_directory}tokens"
  lazy val tfidfs_path: String = s"${tfidfs_directory}tfidfs"
  lazy val errors_path: String = s"${errors_directory}errors"
  lazy val cats: Int = 5
  lazy val docs: Double = cats.toDouble
  lazy val limit: Int = 1000

}


object KaggleComp extends App {

  def store_tokens()(implicit s:PSession) = {
    import Globals._
    val tb: String = table
    //val tokens: Seq[TrainToken] = Queries.get_train_tokens(tb)
    val tokens: Seq[Example] = Queries.get_training(tb)
    val tokens_grouped: ParSeq[Seq[Example]] = tokens.grouped(group_size).toSeq.par
    tokens_grouped.foreach {
      g => {
        val outputfile = tokens_path + tokens_grouped.indexOf(g) + ".csv"
        val f = new java.io.File(outputfile)
        val p = new java.io.PrintWriter(new java.io.FileOutputStream(f, false))
        //val grams_map: Map[(String, Int),Int] = grams.foldLeft(HashMap.empty[(String, Int),Int] withDefaultValue(0)) { (m,x) => m + ((x._1, x._2) -> (1 + m((x._1, x._2)) )) }

        g.foreach {
          t => {
            //val split_tokens = t.tokens.split("<\\^>|<~>").toList
            //val sifted_tokens = sift(split_tokens)
            val split_tokens = t.phrase.split(" ").toList
            val sifted_tokens = sift(split_tokens)
            sifted_tokens.foreach {
              st => {
                val r: String = s"'$dataset_id','$st','sentiment',${t.sentiment},1"
                appendToFile(outputfile, r)
              }
            }
          }
        }
      }
    }
    val dir: String = tokens_directory
    val files: Array[java.io.File] = new java.io.File(dir).listFiles.filter(_.getName.endsWith(".csv"))
    for { f <- files} yield {
      Queries.store_raw(table, f)
    }
  }

  def calculate_counts()(implicit s:PSession) = {
    import Globals._
    Queries.store_counts(table)
  }

  def calculate_tfidfs()(implicit s:PSession) = {
    import Globals._

    val tokens: Seq[TrainingTfidfRow] = Queries.get_train_counts(table)
    val total_terms_in_doc: Map[Int, Int] = tokens.groupBy(_.number).map(x => x._1 -> x._2.size)
    val tokens_grouped: ParSeq[Seq[TrainingTfidfRow]] = tokens.grouped(group_size).toSeq.par
    tokens_grouped.foreach {
      tks => {
        val outputfile = tfidfs_path + tokens_grouped.indexOf(tks) + ".csv"
        val f = new java.io.File(outputfile)
        val p = new java.io.PrintWriter(new java.io.FileOutputStream(f, false))
        for {
          tk <- tks
        } yield {
          val tf: Double = tk.count.toDouble / total_terms_in_doc.getOrElse(tk.number, 1)
          val idf: Double = scala.math.log1p(docs / tks.filter(t => t.token == tk.token).map(_.number).distinct.size)
          val tfidf: Double = tf * idf * 100000d
          val r: String = s"'$dataset_id','${tk.token.getOrElse("").replace("'","&quot;")}','sentiment',${tk.number},${tk.count},$tfidf"
          appendToFile(outputfile, r)
        }
      }
    }

    val dir: String = tfidfs_directory
    val files: Array[java.io.File] = new java.io.File(dir).listFiles.filter(_.getName.endsWith(".csv"))
    for {f <- files} yield {
      Queries.update_tfidf(table, f)
    }
  }

  def validate()(implicit s:PSession) = {
    import Globals._
    val train_examples: Seq[Example] = Queries.get_training_examples(table, limit)
    val outputfile = errors_path + ".csv"
    val f = new java.io.File(outputfile)
    val p = new java.io.PrintWriter(new java.io.FileOutputStream(f, false))
    val train_size: Double = Queries.get_train_size(table).toDouble
    val p_sents: Map[Int, Double] =
      (for {
        i <- 0 until cats
      } yield {
        val sent_size: Int = Queries.get_prob_sent(table, i)
        (i, sent_size / train_size)
      }).toMap
    train_examples.foreach {
      e => {
        val tokens = e.phrase //the phrase has been replaced with its corresponding tokens in Queries.get_training_examples
        //val split_tokens = tokens.split("<\\^>|<~>").toList
        //val sifted_tokens = sift(split_tokens)
        val split_tokens: List[String] = tokens.split(" ").toList.map(x => x.replace("'","&quot;"))
        val sifted_tokens: List[String] = sift(split_tokens)
        val phrase_tfidfs: List[TrainingTfidfRow] = Queries.get_tfidf(table, sifted_tokens).toList
        println(train_examples.indexOf(e))
        val bayesian: Seq[(Int, Double)] =
          for {
            i <- 0 until cats
          } yield {
            val p_for_curr_cat: Seq[Double] =
              for {
                t <- sifted_tokens
              } yield {
                val token_found: Seq[TrainingTfidfRow] = phrase_tfidfs.filter(_.token.getOrElse("") == t)
                val cat_found_in_token: Seq[TrainingTfidfRow] = token_found.filter(x => x.number == i)
                val p_token: Double = token_found.map(x => x.tfidf.getOrElse(0d)).sum
                val p_token_given_sentiment: Double = scala.math.log1p(cat_found_in_token.map(x => x.tfidf.getOrElse(0d)).sum * cat_found_in_token.size + 1d)
                p_token_given_sentiment /*/ p_token*/
              }
            (i, scala.math.pow(p_sents.getOrElse(i, 0d), 2d) * p_for_curr_cat.foldLeft(1d)(_ * _))
          }
        val max_cat: Int = bayesian.maxBy(_._2)._1
        if(max_cat != e.sentiment) {
          val r: String = s"'$dataset_id',${e.phrase_id},'${e.phrase}',${e.sentiment},${max_cat}"
          appendToFile(outputfile, r)
        }
      }
    }
  }

  override def main(args:Array[String]) {
    import Globals._

    DB.withSession {
      implicit session: PSession => {
        //store_tokens()
        //calculate_counts()
        //calculate_tfidfs()
        validate()
      }
    }
  }
}


