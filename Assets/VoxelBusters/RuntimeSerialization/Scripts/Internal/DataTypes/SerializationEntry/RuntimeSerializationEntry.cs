using UnityEngine;
using System.Collections;
using System;

namespace VoxelBusters.RuntimeSerialization.Internal
{
	internal struct RuntimeSerializationEntry 
	{
		#region Properties

		public	string 		Name
		{
			get;
			private set;
		}

		public	object 		Value
		{
			get;
			private set;
		}
		
		public	Type 		Type
		{
			get;
			private set;
		}

		#endregion

		#region Constructors

		internal RuntimeSerializationEntry (string _name, object _value, Type _type) : this ()
		{
			Name	= _name;
			Value	= _value;
			Type	= _type;
		}

		#endregion
	}
}