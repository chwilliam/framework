/*
 * Copyright 2011 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb
package mongodb
package record
package field

import common._
import http.js.JsExp
import http.js.JE.JsNull
import json.JsonAST._
import json.Printer

import net.liftweb.record._
import com.mongodb._

import scala.xml._

/** Field that contains an entire record represented as an inline object value. Inspired by JSONSubRecordField */
class BsonRecordField[OwnerType <: BsonRecord[OwnerType], SubRecordType <: BsonRecord[SubRecordType]]
(rec: OwnerType, valueMeta: BsonMetaRecord[SubRecordType])(implicit subRecordType: Manifest[SubRecordType])
  extends Field[SubRecordType, OwnerType]
  with MandatoryTypedField[SubRecordType]
  with LifecycleCallbacks
{
  def this(rec: OwnerType, valueMeta: BsonMetaRecord[SubRecordType], value: SubRecordType)
          (implicit subRecordType: Manifest[SubRecordType]) = {
    this(rec, value.meta)
    set(value)
  }

  def this(rec: OwnerType, valueMeta: BsonMetaRecord[SubRecordType], value: Box[SubRecordType])
          (implicit subRecordType: Manifest[SubRecordType]) = {
    this(rec, valueMeta)
    setBox(value)
  }

  def owner = rec
  def asJs = asJValue match {
    case JNothing => JsNull
    case jv => new JsExp {
      lazy val toJsCmd = Printer.compact(render(jv))
    }
  }
  def toForm: Box[NodeSeq] = Empty
  def defaultValue = valueMeta.createRecord

  def setFromString(s: String): Box[SubRecordType] = valueMeta.fromJsonString(s)

  def setFromAny(in: Any): Box[SubRecordType] = in match {
    case dbo: DBObject => setBox(Full(valueMeta.fromDBObject(dbo)))
    case _ => genericSetFromAny(in)
  }

  def asJValue: JValue = valueBox.map(_.asJValue) openOr (JNothing: JValue)
  def setFromJValue(jvalue: JValue): Box[SubRecordType] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case _ => setBox(valueMeta.fromJValue(jvalue))
  }

  //Lifecycle Callbacks to the record
  override def beforeValidation { get.meta.foreachCallback( get, _.beforeValidation ) }
  override def afterValidation { get.meta.foreachCallback( get, _.afterValidation ) }

  override def beforeSave { get.meta.foreachCallback( get, _.beforeSave ) }
  override def beforeCreate { get.meta.foreachCallback( get, _.beforeCreate ) }
  override def beforeUpdate { get.meta.foreachCallback( get, _.beforeUpdate ) }

  override def afterSave { get.meta.foreachCallback( get, _.afterSave ) }
  override def afterCreate { get.meta.foreachCallback( get, _.afterCreate ) }
  override def afterUpdate { get.meta.foreachCallback( get, _.afterUpdate ) }

  override def beforeDelete { get.meta.foreachCallback( get, _.beforeDelete ) }
  override def afterDelete { get.meta.foreachCallback( get, _.afterDelete ) }
}

/*
 * List of BsonRecords
 */
class BsonRecordListField[OwnerType <: BsonRecord[OwnerType], SubRecordType <: BsonRecord[SubRecordType]]
(rec: OwnerType, valueMeta: BsonMetaRecord[SubRecordType])(implicit mf: Manifest[SubRecordType])
  extends MongoListField[OwnerType, SubRecordType](rec: OwnerType) with LifecycleCallbacks {

  import scala.collection.JavaConversions._

  override def asDBObject: DBObject = {
    val dbl = new BasicDBList
    value.foreach { v => dbl.add(v.asDBObject) }
    dbl
  }

  override def setFromDBObject(dbo: DBObject): Box[List[SubRecordType]] =
    setBox(Full(dbo.keySet.toList.map(k => {
      valueMeta.fromDBObject(dbo.get(k.toString).asInstanceOf[DBObject])
    })))

  override def asJValue = JArray(value.map(_.asJValue))

  override def setFromJValue(jvalue: JValue) = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JArray(arr) => setBox(Full(arr.map( jv => {
      valueMeta.fromJValue(jv) openOr valueMeta.createRecord
    })))
    case other => setBox(FieldHelpers.expectedA("JArray", other))
  }


  //Lifecycle Callbacks to each record
  override def beforeValidation { get.foreach(  rec => rec.meta.foreachCallback( rec, _.beforeValidation ) ) }
  override def afterValidation { get.foreach(  rec => rec.meta.foreachCallback( rec, _.afterValidation ) ) }

  override def beforeSave { get.foreach(  rec => rec.meta.foreachCallback( rec, _.beforeSave ) ) }
  override def beforeCreate { get.foreach(  rec => rec.meta.foreachCallback( rec, _.beforeCreate ) ) }
  override def beforeUpdate { get.foreach(  rec => rec.meta.foreachCallback( rec, _.beforeUpdate ) ) }

  override def afterSave { get.foreach(  rec => rec.meta.foreachCallback( rec, _.afterSave ) ) }
  override def afterCreate { get.foreach(  rec => rec.meta.foreachCallback( rec, _.afterCreate ) ) }
  override def afterUpdate { get.foreach(  rec => rec.meta.foreachCallback( rec, _.afterUpdate ) ) }

  override def beforeDelete { get.foreach( rec => rec.meta.foreachCallback( rec, _.beforeDelete ) ) }
  override def afterDelete { get.foreach( rec => rec.meta.foreachCallback( rec, _.afterDelete ) ) }
}
