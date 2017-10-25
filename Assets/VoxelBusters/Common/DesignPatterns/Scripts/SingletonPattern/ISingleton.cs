using UnityEngine;
using System.Collections;

namespace VoxelBusters.DesignPatterns
{
	public interface ISingleton 
	{
		/// <summary>
		/// By default, Singleton is meant to be persistent i.e., it wont be destroyed when Application quits.
		/// But incase, if we desire to destroy an Singleton, then this method needs to be called.
		/// </summary>
		void ForceDestroy ();
	}
}