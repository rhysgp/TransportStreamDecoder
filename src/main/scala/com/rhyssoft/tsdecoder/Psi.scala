package com.rhyssoft.tsdecoder



case class Pointer(pointer: Int, fillerBytes: Array[Byte])

case class TableHeader(
  tableId: Int,
  sectionSyntaxIndicator: Boolean,
  privateBit: Boolean,
  reservedBits: Byte,
  sectionLengthUnusedBits: Byte,
  sectionLength: Int,
  syntaxSectionTableData: Array[Byte]
)

case class TableSyntaxSection(
  tableIdExtension: Int,
  reservedBits: Byte,
  versionNumber: Byte,
  currentOrNextIndicator: Boolean,
  sectionNumber: Int,
  lastSectionNumber: Int,
  tableData: Array[Byte],
  crc32: Int
)

case class Descriptor(tag: Int, descriptorLength: Int, data: Array[Byte])

case class Pat(
  programNum: Int,
  reservedBits: Byte,
  programMapPid: Int
)

case class Pmt(
  pcrPid: Int,
  programInfoLength: Int,
  programDescriptors: Array[Byte],
  elementaryStreamInfoData: Array[Byte]
)

case class ElementaryStreamData(
  streamType: Int,
  elementaryPid: Int,
  esInfoLength: Int,
  elementaryStreamDescriptors: Array[Byte]
)

object TableIdentifiers {
  val programAssociationSection = 0
  val conditionalAccessSection = 1
  val programMapSection = 2
  val transportStreamDescription = 3
  val sceneDescriptionSection = 4
  val objectDescriptionSection = 5
  val metadataSection = 6
  val ipmpControlInformation = 7
  val ccMultiProtocolEncapsulated = 58
  val ccUnMessage = 59
  val ccDownloadDataMessage = 60
  val ccStreamDescriptorList = 61
  val ccPrivatelyDefined = 62
  val ccAddressable = 63
  // etc

//  0	0x00	Program Association section contains a directory listing of all Program Map Tables
//  1	0x01	Conditional Access section contains a directory listing of all EMM streams
//  2	0x02	Program Map section contains a directory listing of all elementary streams.
//  3	0x03	Transport Stream Description section.
//  4	0x04	ISO/IEC 14496 scene description section.
//  5	0x05	ISO/IEC 14496 object description section.
//  6	0x06	Metadata section.
//  7	0x07	ISO/IEC 13818-11 IPMP control information (DRM).
//  8 - 57	0x08 - 0x39	Reserved.
//  58	0x3A	ISO/IEC 13818-6 DSM CC multiprotocol encapsulated.
//  59	0x3B	ISO/IEC 13818-6 DSM CC U-N messages.
//  60	0x3C	ISO/IEC 13818-6 DSM CC Download Data Messages.
//  61	0x3D	ISO/IEC 13818-6 DSM CC stream descriptor list.
//  62	0x3E	ISO/IEC 13818-6 DSM CC privately defined (DVB MAC addressed datagram).
//  63	0x3F	ISO/IEC 13818-6 DSM CC addressable.
//  64 - 127	0x40 - 0x7F	Used by DVB.
//  128 - 143	0x80 - 0x8F	DVB-CSA and DigiCipher II/ATSC CA message sections used in EMM and ECM streams.
//  144 - 191	0x90 - 0xBF	May be assigned as needed to other data tables.
//  192 - 254	0xC0 - 0xFE	Used by DigiCipher II/ATSC/SCTE.
//  255	0xFF	Forbidden. As is used for null padding.
}
