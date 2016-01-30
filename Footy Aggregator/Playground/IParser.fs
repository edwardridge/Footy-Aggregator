module IParser

open Aggregator

type IParser =
    abstract member parse: string -> Result list