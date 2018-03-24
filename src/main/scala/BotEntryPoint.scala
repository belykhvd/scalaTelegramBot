package TelegramBot

import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.methods.ParseMode

object BotEntryPoint extends TelegramBot with Polling with Commands {
  lazy val token = "406632590:AAHtcW8lFqtIhHZLP9OqXXjmnHU4tflEkbE"

  private val botEngine = BotEngine

  onCommand('create_poll) {
    implicit msg => withArgs { args =>
      reply(botEngine.create_poll(restoreArgString(args), msg.from.get.id))
    }}

  onCommand('delete_poll) {
    implicit msg => withArgs { args =>
      reply(botEngine.delete_poll(restoreArgString(args), msg.from.get.id))
    }}

  onCommand('list) {
    implicit msg => withArgs { args =>
      reply(botEngine.list(msg.from.get.id))
    }}

  onCommand('start_poll) {
    implicit msg => withArgs { args =>
      reply(botEngine.start_poll(restoreArgString(args), msg.from.get.id))
    }}

  onCommand('stop_poll) {
    implicit msg => withArgs { args =>
      reply(botEngine.stop_poll(restoreArgString(args), msg.from.get.id))
    }}

  onCommand('result) {
    implicit msg => withArgs { args =>
      reply(botEngine.result(restoreArgString(args), msg.from.get.id), parseMode = Some(ParseMode.Markdown))
    }}

  onCommand('begin) {
    implicit msg => withArgs { args =>
      reply(botEngine.begin(restoreArgString(args), msg.from.get.id))
    }}

  onCommand('add_question) {
    implicit msg => withArgs { args =>
      reply(botEngine.add_question(restoreArgString(args), msg.from.get.id))
    }}

  onCommand('delete_question) {
    implicit msg => withArgs { args =>
      reply(botEngine.delete_question(restoreArgString(args), msg.from.get.id))
    }}

  onCommand('answer) {
    implicit msg => withArgs { args =>
      reply(botEngine.answer(restoreArgString(args), msg.from.get.id))
    }}

  onCommand('view) {
    implicit msg => withArgs { args =>
      reply(botEngine.view(msg.from.get.id))
    }}

  onCommand('end) {
    implicit msg => withArgs { args =>
      reply(botEngine.end(msg.from.get.id))
    }}

  private def restoreArgString(seq: Seq[String]): String = seq.mkString(" ")
}