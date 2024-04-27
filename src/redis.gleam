import gleam/io

// Uncomment this block to pass the first stage
//
import flash.{GroupAttr, InfoLevel, StringAttr}
import gleam/bit_array
import gleam/bytes_builder.{type BytesBuilder}
import gleam/erlang/process
import gleam/option.{None}
import gleam/otp/actor
import gleam/result
import glisten.{Packet}

pub fn main() {
  let logger = build_logger()
  io.println("Logs from your program will appear here!")

  let assert Ok(_) =
    glisten.handler(init, fn(msg, state, conn) {
      loop(logger, msg, state, conn)
    })
    |> glisten.serve(6379)

  process.sleep_forever()
}

fn init(_conn) {
  #(Nil, None)
}

fn loop(logger, msg, state, conn) {
  let assert Packet(msg) = msg
  let assert command = bit_array.to_string(msg)

  logger
  |> flash.with_group("command")
  |> flash.with_attrs([
    StringAttr("RESP_command", result.unwrap(command, "unknown")),
  ])
  |> flash.info("received command")

  let response = build_pong_msg()
  let assert Ok(_) = glisten.send(conn, response)

  logger
  |> flash.with_group("response")
  |> flash.with_attrs([
    StringAttr(
      "RESP_response",
      result.unwrap(
        response
          |> bytes_builder.to_bit_array
          |> bit_array.to_string,
        "unknown",
      ),
    ),
  ])
  |> flash.info("sent response")

  actor.continue(state)
}

fn build_pong_msg() -> BytesBuilder {
  bytes_builder.from_string("+PONG\r\n")
}

fn build_logger() {
  flash.new(InfoLevel, flash.json_writer)
}
