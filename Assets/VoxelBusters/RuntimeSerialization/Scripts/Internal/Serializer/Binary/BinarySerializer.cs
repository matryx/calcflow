using UnityEngine;
using System.Collections;
using System;
using System.IO;
using System.Collections.Generic;

namespace VoxelBusters.RuntimeSerialization.Internal
{
	internal class BinarySerializer 
	{
		#region Properties

		protected RSBinaryWriter BinaryStreamWriter
		{
			get;
			private set;
		}

		protected ObjectWriter ObjectDataWriter
		{
			get;
			private set;
		}

		#endregion

		#region Constructors

		internal BinarySerializer ()
		{
			BinaryStreamWriter			= new RSBinaryWriter(512);
			int	_SFVersion				= Constants.kSerializationFormatVersion;

			// Based of serialization format select object writer
			switch (_SFVersion)
			{
			case 1:
				ObjectDataWriter		= new ObjectWriterSFV1();
				break;

#pragma warning disable
				default:
					throw new Exception("[RS] Unimplemented serialization format version found!");
#pragma warning restore
			}
		}

		~BinarySerializer()
		{
			if (BinaryStreamWriter != null)
				BinaryStreamWriter.Close();

			BinaryStreamWriter			= null;
			ObjectDataWriter			= null;
		}

		#endregion

		#region Serialize Methods
		
		internal byte[] Serialize (object _object, Type _objectType)
		{
			try
			{
				// Serialize version info
				BinaryStreamWriter.WriteBinaryElement(BinaryElement.VERSION);
				BinaryStreamWriter.Write(Constants.kSerializationFormatVersion);
				
				// Serialize object value
				ObjectDataWriter.WriteObjectValue(BinaryStreamWriter, _object);
				
				// Get serialization data
				byte[]	_serializationData	= BinaryStreamWriter.ToBytes();

				return _serializationData;
			}
			finally
			{
				Reset();
			}
		}

		#endregion

		#region Methods

		private void Reset ()
		{
			if (BinaryStreamWriter == null)
				throw new Exception("[RS] Binary writer is null.");

			if (ObjectDataWriter == null)
				throw new Exception("[RS] Object writer is null.");

			// Reset
			BinaryStreamWriter.Reset();
			ObjectDataWriter.Reset();
			UnityObjectSerializationUtil.Reset();
		}

		#endregion
	}
}