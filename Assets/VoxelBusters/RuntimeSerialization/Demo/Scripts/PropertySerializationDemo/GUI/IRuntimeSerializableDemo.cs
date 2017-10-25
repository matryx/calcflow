using UnityEngine;
using System.Collections;
using System;
using VoxelBusters.RuntimeSerialization;

namespace VoxelBusters.RuntimeSerialization.Demo
{
	public class IRuntimeSerializableDemo : SerializationDemo 
	{
		[RuntimeSerializable]
		public class IRuntimeSerializableSample : SerializationSample, IRuntimeSerializable, IRuntimeSerializableActivator
		{
			#region Static Methods

			public static object CreateInstance (RuntimeSerializationInfo _info)
			{
				return new IRuntimeSerializableSample();
			}

			#endregion

			#region Callbacks
			
			public void WriteSerializationData (RuntimeSerializationInfo _info)
			{
				_info.AddValue<string>("string", stringField);
				_info.AddValue<int>("int", intField);
				_info.AddValue<Vector3>("vec3", vec3Field);
				_info.AddValue<DateTime>("date-time", dateTimeField);
				_info.AddValue<eSamepleEnum>("enum", EnumField);
			}
			
			public object ReadSerializationData (RuntimeSerializationInfo _info)
			{
				stringField		= _info.GetValue<string>("string");
				intField		= _info.GetValue<int>("int");
				vec3Field		= _info.GetValue<Vector3>("vec3");
				dateTimeField	= _info.GetValue<DateTime>("date-time");
				EnumField		= _info.GetValue<eSamepleEnum>("enum");

				return this;
			}

			#endregion
		}

		#region Constants
		
		private 	const 		string 		kSave2PrefsSerializationID		= "rs-interface-prefs";
		private 	const 		string 		kSave2FileSerializationID		= "rs-interface-file";

		#endregion

		#region Properties
		
		public 		override	string		Save2PrefsSerializationID
		{
			get
			{
				return kSave2PrefsSerializationID;
			}
		}

		public 		override	string		Save2FileSerializationID
		{
			get
			{
				return kSave2FileSerializationID;
			}
		}
		
		#endregion
		
		#region Method
		
		protected override SerializationSample CreateNewSampleInstance ()
		{
			return new IRuntimeSerializableSample();
		}
		
		#endregion 
	}
}