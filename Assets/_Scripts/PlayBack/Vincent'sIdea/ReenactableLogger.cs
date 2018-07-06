using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class ReenactableLogger : MonoBehaviour
{
   public ReenactableType GetReeneactableType()
    {
		return type;
    }
   protected abstract ReenactableType type { get; }

}
