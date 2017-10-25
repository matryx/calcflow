using UnityEngine;
using System.Collections;

namespace VoxelBusters.DesignPatterns
{
	public interface IObserver 
	{
		/// <summary>
		/// When property of subject changes, an event is triggered which notifies all the observers.
		/// Wherein changed value is identified by _key and changed value is packed within _data
		/// </summary>
		/// <param name="_key">_key.</param>
		/// <param name="_data">_data.</param>
		void OnPropertyChange (string _key, ArrayList _data);
	}
}
