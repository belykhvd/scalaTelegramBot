package TelegramBot

import java.util.UUID

object PollUUIDGenerator {
  def next: Long = UUID.randomUUID().getMostSignificantBits & Long.MaxValue
}