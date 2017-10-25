using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

namespace VoxelBusters.RuntimeSerialization.Demo
{
	public class AttributesSerializationDemo : SerializationDemo
	{
		[RuntimeSerializable]
		public class AttributesSerializationSample : SerializationSample
		{}

		#region Constants
		
		private 	const 		string 		kSave2PrefsSerializationID		= "rs-attributes-prefs";
		private 	const 		string 		kSave2FileSerializationID		= "rs-attributes-file";
		
		#endregion
		
		#region Properties
		
		public 		override	string		Save2PrefsSerializationID
		{
			get
			{
				return kSave2PrefsSerializationID;
			}
		}
		
		public 	override		string		Save2FileSerializationID
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
			return new AttributesSerializationSample();
		}
		
		#endregion 
	}
}