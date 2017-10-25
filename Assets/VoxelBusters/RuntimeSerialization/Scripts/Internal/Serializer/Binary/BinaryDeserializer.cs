using UnityEngine;
using System.Collections;
using System;
using System.IO;
using System.Collections.Generic;

namespace VoxelBusters.RuntimeSerialization.Internal
{
	internal class BinaryDeserializer
	{
		#region Properties
		
		protected RSBinaryReader BinaryStreamReader
		{
			get;
			private set;
		}
		
		protected ObjectReader[] ObjectReaderList
		{
			get;
			private set;
		}
		
		#endregion

		#region Constructors
		
		internal BinaryDeserializer ()
		{
			BinaryStreamReader		= new RSBinaryReader(512);

			// Load all the supported object readers
			ObjectReaderList		= new ObjectReader[Constants.kSerializationSupportedFormatVersions];
			ObjectReaderList[0]		= new ObjectReaderSFV1();
		}

		~BinaryDeserializer ()
		{
			if (BinaryStreamReader != null)
				BinaryStreamReader.Close();

			BinaryStreamReader		= null;
			ObjectReaderList		= null;
		}
		
		#endregion

		#region Deserialize Methods
		
		internal T Deserialize <T> (byte[] _serializationData, T _targetObject)
		{
			try
			{
				// Check serialization data
				if (_serializationData == null)
					throw new Exception("[RS] Serialization data is null.");
				
				// Load serialization data to stream buffer
				BinaryStreamReader.LoadBuffer(_serializationData);
				
				// Get version info
				int 			_serializationFormatVersion	= GetVersionInfo();
				
				// Check if serialization version is compatible
				if (_serializationFormatVersion > Constants.kSerializationSupportedFormatVersions)
					throw new Exception("[RS] Failed to deserialize object. Serialized data format not supported please update RS plugin to the most recent version.");
				
				ObjectReader 	_objectReader				= GetObjectReader(_serializationFormatVersion);
			
				// Start deserializing data
				Type 			_deserializedType;
				object 			_deserializedObj			= _objectReader.ReadObjectValue(BinaryStreamReader, out _deserializedType, _targetObject);
				
				if (_deserializedObj == null)
				{
					return default(T);
				}
				else if (typeof(T).IsInstanceOfType(_deserializedObj))
				{	
					return (T)_deserializedObj;
				}
				else 
				{
					throw new Exception(string.Format("[RS] Deserialized object isnt an object of type={0}.", typeof(T)));
				}
			}
			finally
			{
				Reset();
			}
		}
		
		internal int GetVersion (byte[] _serializationData)
		{
			try
			{
				// Check serialization data
				if (_serializationData == null)
					throw new Exception("[RS] Serialization data is null.");
				
				// Load serialization data to stream buffer
				BinaryStreamReader.LoadBuffer(_serializationData);
				
				// Get version info
				return GetVersionInfo();
			}
			finally
			{
				Reset();
			}
		}
		
		private int GetVersionInfo ()
		{
			if (BinaryStreamReader.ReadBinaryElement() != BinaryElement.VERSION)
				throw new Exception("[RS] Failed to get version info. BinaryElement.VERSION not found.");
			
			return BinaryStreamReader.ReadInt32();
		}

		#endregion

		#region Methods

		private void Reset ()
		{ 
			if (BinaryStreamReader == null)
				throw new Exception("[RS] Binary writer is null.");
			
			if (ObjectReaderList == null)
				throw new Exception("[RS] Object reader is null.");
			
			// Reset
			BinaryStreamReader.Reset();
			UnityObjectSerializationUtil.Reset();

			int _count	= ObjectReaderList.Length;

			for (int _iter = 0; _iter < _count; _iter++)
				ObjectReaderList[_iter].Reset();
		}

		private ObjectReader GetObjectReader (int _formatVersion)
		{
			return ObjectReaderList[_formatVersion - 1];
		}

		#endregion
	}
}