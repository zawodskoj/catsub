import zio.json.*
import zio.json.ast.*

package object models {

  case class Chat(id: Long)

  object Chat {
    given JsonCodec[Chat] = DeriveJsonCodec.gen
  }

  case class Sticker(set_name: Option[String])

  object Sticker {
    given JsonCodec[Sticker] = DeriveJsonCodec.gen
  }

  case class Message(
    message_id: Long,
    date: Long,
    chat: Chat,
    from: Chat,
    reply_to_message: Option[Message],
    text: Option[String],
    caption: Option[String],
    sticker: Option[Sticker],
    entities: Option[List[MessageEntity]],
    photo: Option[Json]
  ) {
    def textOrCaption: Option[String] = text.orElse(caption)
  }
  object Message {
    given JsonCodec[Message] = DeriveJsonCodec.gen
  }

  case class Update(update_id: Long, message: Option[Message], edited_message: Option[Message])

  object Update {
    given JsonCodec[Update] = DeriveJsonCodec.gen
  }

  case class GetUpdatesResponse(result: Vector[Update])

  object GetUpdatesResponse {
    given JsonCodec[GetUpdatesResponse] = DeriveJsonCodec.gen
  }

  case class SendMessageResponse(result: Message)

  object SendMessageResponse {
    given JsonCodec[SendMessageResponse] = DeriveJsonCodec.gen
  }

  case class MessageEntity(`type`: String)

  object MessageEntity {
    given JsonCodec[MessageEntity] = DeriveJsonCodec.gen
  }
}
