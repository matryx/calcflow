using UnityEngine;
using System.Collections;
using System;
using System.IO;
using VoxelBusters.RuntimeSerialization;

namespace VoxelBusters.RuntimeSerialization.Demo
{
	[Serializable, RuntimeSerializable]
	public class SerializationSample
	{
		public enum eSamepleEnum
		{
			VAL_1,
			VAL_2
		}
		
		#region Properties

		// internal fields
		public	 		string			stringField			= "string value";
		public			int				intField			= 1;
		public			Vector3			vec3Field			= Vector3.one;
		public			DateTime		dateTimeField;

		// Private fields
		[RuntimeSerializeField]
		private			eSamepleEnum	m_enumField;
		public			eSamepleEnum	EnumField
		{
			get 
			{
				return m_enumField;
			}

			set
			{
				m_enumField	= value;
			}
		}

		#endregion

		#region Methods

		public void AssignRandomValue ()
		{
			stringField			= Path.GetRandomFileName().Replace(".", "");
			intField			= UnityEngine.Random.Range(0, 1000);
			vec3Field			= UnityEngine.Random.insideUnitSphere * 5;

			// Random date
			DateTime _startDate	= new DateTime(1947, 1, 1);
			int _range 			= (DateTime.Today - _startDate).Days;           
			dateTimeField		= _startDate.AddDays(UnityEngine.Random.Range(0, _range));

			// Random enum
			m_enumField			= (UnityEngine.Random.value > 0.5f) ? eSamepleEnum.VAL_2 : eSamepleEnum.VAL_1;
		}

		public override string ToString ()
		{
			return string.Format ("StringField={0}\n IntField={1}\n Vec3Field={2}\n DateTimeField={3}\n EnumField={4}", 
			                      stringField, intField, vec3Field, dateTimeField, m_enumField);
		}

		#endregion
	}
}