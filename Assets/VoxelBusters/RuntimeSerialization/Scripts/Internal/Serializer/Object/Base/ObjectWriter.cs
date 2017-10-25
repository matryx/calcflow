using UnityEngine;
using System.Collections;
using System;
using System.Collections.Generic;

namespace VoxelBusters.RuntimeSerialization.Internal
{
	internal abstract class ObjectWriter
	{
		#region Properties
		
		protected UInt32 ObjectReferenceCounter
		{
			get;
			set;
		}

		protected Dictionary<object, UInt32> ObjectReferenceCache
		{
			get;
			private set;
		}

		protected SerializationTypeMetadata TypeMetadata
		{
			get;
			private set;
		}

		#endregion
		
		#region Constructors
		
		internal ObjectWriter ()
		{
			ObjectReferenceCounter		= 0;
			ObjectReferenceCache		= new Dictionary<object, UInt32>();
			TypeMetadata				= new SerializationTypeMetadata();
		}

		~ObjectWriter ()
		{
			ObjectReferenceCache		= null;
			TypeMetadata				= null;
		}
		
		#endregion
		
		#region Abstract Methods
		
		internal abstract void WriteObjectValue (RSBinaryWriter _binaryWriter, object _object);

		#endregion

		#region Methods

		internal void Reset ()
		{
			// Resetting object reference cache
			ObjectReferenceCounter		= 0;
			ObjectReferenceCache.Clear();

			// Resetting type metadata
			TypeMetadata.Reset();
		}

		#endregion
	}
}