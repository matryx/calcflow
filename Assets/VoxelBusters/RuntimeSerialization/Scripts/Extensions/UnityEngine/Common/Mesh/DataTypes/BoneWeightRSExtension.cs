using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

public class BoneWeightRSExtension : IRuntimeSerializableExtension
{
	#region Constants
	
	private 	const	string		kBoneIndex0Key		= "b0";
	private 	const	string		kBoneIndex1Key		= "b1";
	private 	const	string		kBoneIndex2Key		= "b2";
	private 	const	string		kBoneIndex3Key		= "b3";
	private 	const	string		kWeight0Key			= "w0";
	private 	const	string		kWeight1Key			= "w1";
	private 	const	string		kWeight2Key			= "w2";
	private 	const	string		kWeight3Key			= "w3";

	#endregion
	
	#region Serialization Methods
	
	public override void WriteSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		BoneWeight	_boneWeight	= (BoneWeight)_object;
		
		// Serialize properties
		_info.AddValue<int>(kBoneIndex0Key,	_boneWeight.boneIndex0);
		_info.AddValue<int>(kBoneIndex1Key,	_boneWeight.boneIndex1);
		_info.AddValue<int>(kBoneIndex2Key,	_boneWeight.boneIndex2);
		_info.AddValue<int>(kBoneIndex3Key,	_boneWeight.boneIndex3);
		_info.AddValue<float>(kWeight0Key,	_boneWeight.weight0);
		_info.AddValue<float>(kWeight1Key,	_boneWeight.weight1);
		_info.AddValue<float>(kWeight2Key,	_boneWeight.weight2);
		_info.AddValue<float>(kWeight3Key,	_boneWeight.weight3);
	}
	
	public override object ReadSerializationData (object _object, RuntimeSerializationInfo _info)
	{
		BoneWeight	_boneWeight	= (BoneWeight)_object;

		// Deserialize properties
		_boneWeight.boneIndex0	= _info.GetValue<int>(kBoneIndex0Key);
		_boneWeight.boneIndex1	= _info.GetValue<int>(kBoneIndex1Key);
		_boneWeight.boneIndex2	= _info.GetValue<int>(kBoneIndex2Key);
		_boneWeight.boneIndex3	= _info.GetValue<int>(kBoneIndex3Key);
		_boneWeight.weight0		= _info.GetValue<float>(kWeight0Key);
		_boneWeight.weight1		= _info.GetValue<float>(kWeight1Key);
		_boneWeight.weight2		= _info.GetValue<float>(kWeight2Key);
		_boneWeight.weight3		= _info.GetValue<float>(kWeight3Key);

		return _boneWeight;
	}
	
	#endregion
}