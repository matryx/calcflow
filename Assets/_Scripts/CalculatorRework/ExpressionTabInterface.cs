using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public interface ExpressionTabInterface
{
    ExpressionSet getExpSet();
    Transform getExpressionX();
    void setExpressionX(Transform e);
    void setSeparator(Transform sep);
    void deleteExpressionFromScroll();
    bool getActiveStatus();
}
