using UnityEngine;
using System.Collections;
using System;
using System.Collections.Generic;

namespace VoxelBusters.RuntimeSerialization.Internal
{
	internal abstract class ObjectReader 
	{
		#region Properties
		
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

		internal ObjectReader ()
		{
			ObjectReferenceCache	= new Dictionary<object, UInt32>();
			TypeMetadata			= new SerializationTypeMetadata();
		}

		~ObjectReader ()
		{
			ObjectReferenceCache	= null;
			TypeMetadata			= null;
		}
		
		#endregion
		
		#region Abstract Methods
		
		internal abstract object ReadObjectValue (RSBinaryReader _binaryReader, out Type _objectType, object _object = null);

		#endregion

		#region Methods

		internal virtual void Reset ()
		{
			// Resetting object reference cache
			ObjectReferenceCache.Clear();
			
			// Resetting type metadata
			TypeMetadata.Reset();
		}

		#endregion
	}
}