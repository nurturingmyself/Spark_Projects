package com.engage.default

import org.apache.spark.sql.Row
// $example on:init_session$
import org.apache.spark.sql.SparkSession
// $example off:init_session$
// $example on:programmatic_schema$
// $example on:data_types$
import org.apache.spark.sql.types._
// $example off:data_types$
// $example off:programmatic_schema$

object SparkSQLExample {

  // $example on:create_ds$
  case class Person(name: String, age: Long)

  // $example off:create_ds$

  def main(args: Array[String]) {
    // $example on:init_session$
    val spark = SparkSession
      .builder()
      .appName("Spark SQL basic example")
      .config("spark.some.config.option", "some-value")
      .getOrCreate()

    // For implicit conversions like converting RDDs to DataFrames
    import spark.implicits._
    // $example off:init_session$

    /*runBasicDataFrameExample(spark)
    runDatasetCreationExample(spark)
    runInferSchemaExample(spark)
    runProgrammaticSchemaExample(spark)*/

    spark.stop()
  }
}
