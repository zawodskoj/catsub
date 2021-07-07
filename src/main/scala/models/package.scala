import derevo.derive
import derevo.circe._

package object models {
  @derive(codec) case class Chat(id: Long)
  @derive(codec) case class Message(message_id: Long, chat: Chat, reply_to_message: Option[Message], text: Option[String])
  @derive(codec) case class Update(update_id: Long, message: Option[Message], edited_message: Option[Message])
  @derive(codec) case class GetUpdatesResponse(result: Vector[Update])
  @derive(codec) case class SendMessageResponse(result: Message)
}
