package net.liftweb
package mongodb
package record
package field

import net.liftweb.record._
import json._
import common._
import com.mongodb.{BasicDBList, DBObject}
import common.Full
import scala.Some

import scala.collection.JavaConversions._




/**
 * User: chris
 * 2013 - JouleBug
 
 */
 
class MongoFieldList[OwnerType <: BsonRecord[OwnerType], ListType : Manifest](rec: OwnerType, baseField : Field[ListType, OwnerType])
    extends MongoListField[OwnerType, ListType](rec: OwnerType)
{

  def getField = baseField



  override def setFromAny(in: Any): Box[MyType] = {
    in match {
      case dbo: DBObject => setFromDBObject(dbo)
      case list@c::xs if mf.erasure.isInstance(c) => setBox(Full(list.asInstanceOf[MyType]))
      case Some(list@c::xs) if mf.erasure.isInstance(c) => setBox(Full(list.asInstanceOf[MyType]))
      case Full(list@c::xs) if mf.erasure.isInstance(c) => setBox(Full(list.asInstanceOf[MyType]))
      case s: String => setFromString(s)
      case Some(s: String) => setFromString(s)
      case Full(s: String) => setFromString(s)
      case null|None|Empty => setBox(defaultValueBox)
      case f: Failure => setBox(f)
      case o => setFromString(o.toString)
    }
  }

  override def setFromJValue(jvalue: JValue) = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JArray(arr) => setBox(Full(arr.flatMap( value => baseField.setFromJValue( value ) )))
    case other => setBox(FieldHelpers.expectedA("JArray", other))
  }

  override def asJValue = JArray(value.map(li => {
    val field = getField
    field.setFromAny( li )
    field.asJValue
  }))

  override def asDBObject: DBObject = {
    val dbl = new BasicDBList

    val field = getField

    value.foreach { item => field.setFromAny(item); dbl.add( rec.meta.fieldDbValue(field) )  }

    dbl
  }


  // set this field's value using a DBObject returned from Mongo.
  override def setFromDBObject(dbo: DBObject): Box[MyType] = {

    val field = getField

    setBox(Full(dbo.asInstanceOf[BasicDBList].toList.flatMap( value => field.setFromAny(value ) ) ))
  }

}


